from numpy.distutils.system_info import agg2_info
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import BernoulliDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Procedure, Sequence, VariableAssignment, ActionExecution, Variable, Iterate
from salma.model.process import TriggeredProcess, PeriodicProcess
from salma.test.emobility.map_generator import MapGenerator
from salma.test.emobility.map_translator import MapTranslator
from salma.test.emobility.visualizer import Visualizer
import unittest
from salma.model.world import World
from salma.engine import EclipseCLPEngine
from salma import SALMAException
import logging
import salma
import os
import random
import matplotlib.pyplot as plt
import networkx as nx
import pyclp
from salma.test.emobility.emobility_test import EMobilityTest
import salma.test.emobility.utils as utils
from salma.statistics import SequentialProbabilityRatioTest
from statsmodels.stats import proportion


HYPTEST, ESTIMATION = range(2)

_MODE = ESTIMATION

_VISUALIZE = False



def create_navigation_functions(world_map, mt):
    def possible_target_chooser(agent=None, currentTargetPOI=None, **ctx):
        target_poi = currentTargetPOI(agent.id)
        x, y = mt.get_position_from_node(target_poi)
        target = mt.find_k_closest_nodes(x, y, 3, loctype="plcs")
        return target

    def route_finder(agent=None, vehiclePosition=None, currentTargetPLCS=None, **ctx):
        pos = vehiclePosition(agent.id)
        target = currentTargetPLCS(agent.id)
        r = nx.shortest_path(world_map, pos[1], target)
        return r

    def response_selector(agent=None, vehicle_plcssam_reservationResponses=None, **ctx):
        responses = vehicle_plcssam_reservationResponses(agent.id, "sam1")
        # for now: ignore any time information
        # format: rresp(StartTime, PlannedDuration, BestPLCS)
        if len(responses) > 0:
            return responses[0][3]
        else:
            return None

    return possible_target_chooser, route_finder, response_selector


def create_plcssam_functions(world_map, mt):
    def process_requests_plcs(agent=None, plcssam_vehicle_reservationRequests=None, **ctx):
        """
        :type agent: salma.model.agent.Agent
        :type plcssam_vehicle_reservationRequests: str -> list
        :rtype: list[(str,str)]
        """
        #: :type : EvaluationContext
        ec = agent.evaluation_context

        #: :type: list[(str, str, list of str, int, int)]
        requests = plcssam_vehicle_reservationRequests(agent.id)
        # format: rreq(Vehicle, Alternatives, StartTime, PlannedDuration)
        schedule = []
        assignment = dict()
        for r in requests:
            schedule.append((r[1], r[2]))
        success = utils.choose_alternative(schedule, assignment)
        if not success:
            return []
        result = []
        for vehicle, plcs in assignment.items():
            result.append((vehicle, plcs))

        #todo: establish communication between SAM and PLCs to check availability
        return result

    return process_requests_plcs


class EMobilityScenario3(EMobilityTest):
    NUM_OF_VEHICLES = 3

    def create_vehicles(self, world, world_map, mt):
        target_chooser, route_finder, response_selector = create_navigation_functions(world_map, mt)

        p_request_plcs = Procedure("main", [],
                                   [
                                       VariableAssignment("possible_targets",
                                                          EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                          target_chooser, []),
                                       ActionExecution("queryPLCSSAM",
                                                       [Entity.SELF, "sam1", Variable("possible_targets"),
                                                        0, 0])
                                   ])

        p_set_target = Procedure("main", [],
                                 [
                                     VariableAssignment("sam_response", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                        response_selector, []),
                                     ActionExecution("remove_all_vehicle_plcssam_reservationResponses",
                                                     [Entity.SELF, "sam1"]),
                                     ActionExecution("setTargetPLCS", [Entity.SELF, Variable("sam_response")])
                                 ])

        p_find_route = Procedure("main", [],
                                 [
                                     VariableAssignment("route", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                        route_finder, []),
                                     ActionExecution("setRoute", [Entity.SELF, Variable("route")])
                                 ])

        p_comm_sam = Procedure("main", [],
                               ActionExecution("start_exchange_PLCSSAM_Vehicle", [Entity.SELF, "sam1"]))
        for i in range(EMobilityScenario3.NUM_OF_VEHICLES):
            p1 = TriggeredProcess(p_request_plcs, EvaluationContext.PYTHON_EXPRESSION,
                                  "currentTargetPLCS(self) == 'none' and "
                                  "len(vehicle_plcssam_reservationRequests(self,'sam1')) == 0", [])

            p2 = TriggeredProcess(p_find_route, EvaluationContext.PYTHON_EXPRESSION,
                                  "len(currentRoute(self)) == 0 and currentTargetPLCS(self) != 'none'", [])

            p3 = TriggeredProcess(p_set_target, EvaluationContext.PYTHON_EXPRESSION,
                                  "len(vehicle_plcssam_reservationResponses(self,'sam1')) > 0", [])

            p4 = PeriodicProcess(p_comm_sam, 5)
            vehicle = Agent("v" + str(i), "vehicle", [p1, p2, p3, p4])
            world.addAgent(vehicle)

    def create_plcssam(self, world, world_map, mt):
        request_processor = create_plcssam_functions(world_map, mt)

        p_process_requests = Procedure("main", [],
                                       [
                                           VariableAssignment("assignments", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                              request_processor, []),
                                           Iterate(EvaluationContext.ITERATOR, Variable("assignments"),
                                                   [("v", "vehicle"), ("p", "plcs")],
                                                   ActionExecution("set_plcssam_vehicle_reservationResponse",
                                                                   [Entity.SELF, Variable("v"), 0, 0, Variable("p")])
                                           ),
                                           ActionExecution("remove_all_plcssam_vehicle_reservationRequests",
                                                           [Entity.SELF])
                                       ])

        p1 = TriggeredProcess(p_process_requests, EvaluationContext.PYTHON_EXPRESSION,
                              "len(plcssam_vehicle_reservationRequests(self)) > 0", [])

        sam = Agent("sam1", "plcssam", [p1])
        world.addAgent(sam)

    def __print_info(self, world):
        """
        :param World world: the world
        """
        uninitialized_fluent_instances, uninitialized_constant_instances = world.check_fluent_initialization()
        print("-" * 80)
        print("Uninitialized Fluents:")
        print(uninitialized_fluent_instances)
        print("-" * 80)

        print("Uninitialized Constants:")
        print(uninitialized_constant_instances)
        problematic_stochastic_actions, problematic_exogenous_actions = world.check_action_initialization()
        print("-" * 80)
        print("Uninitialized stochastic actions:")
        print(problematic_stochastic_actions)
        print("-" * 80)
        print("Uninitialized exogenous actions:")
        print(problematic_exogenous_actions)
        world.printState()
        p1, p2 = world.check_action_initialization()
        print(p1)
        print(p2)

    def test_scenario3(self):
        world = World.instance()
        log = False

        mgen = MapGenerator(world)
        world_map = mgen.load_from_graphml("testdata/test1.graphml")

        mt = MapTranslator(world_map, world)
        self.create_plcssam(world, world_map, mt)
        self.create_vehicles(world, world_map, mt)
        self.init_map_and_defaults(world, world_map, mt)

        vehicles = world.getDomain("vehicle")
        crossings = list(world.getDomain("crossing"))
        starts = crossings.copy()
        pois = list(world.getDomain("poi"))
        target_pois = pois.copy()
        sams = world.getDomain("plcssam")
        plcses = world.getDomain("plcs")

        for plcs in plcses:
            world.setConstantValue("maxCapacty", [plcs.id], 10)

        for vehicle in vehicles:
            start = random.choice(starts)
            starts.remove(start)
            target_poi = random.choice(target_pois)
            target_pois.remove(target_poi)

            world.setFluentValue("vehiclePosition", [vehicle.id], ("pos", start.id, start.id, 0))
            world.setFluentValue("vehicleSpeed", [vehicle.id], 10)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi.id)
            world.setConstantValue("calendar", [vehicle.id], [("cal", target_poi.id, 100, 100)])

        world.get_exogenous_action(
            "exchange_PLCSSAM_Vehicle").config.set_probability(0.2)

        fstr = """
        forall([v,vehicle],
            implies(
                occur(queryPLCSSAM(v,?,?,?,?)),
                until(10,
                    true,
                    currentTargetPLCS(v) \= none
                )
            )
        )
        """
        gstr = "forall([v,vehicle], arrive_at_targetPLCS(v))"
        world.registerProperty("f", fstr, World.INVARIANT)
        world.registerProperty("g", gstr, World.ACHIEVE)
        if log:
            self.__print_info(world)

        #verdict, results = self.run_experiment(world, world_map, log=False, visualize=False)
        # print("Verdict: {}\nResults:\n{}".format(verdict, results))
        #results, infos = world.run_repetitions(100)

        # assumption success prob = 0.6 --> H0: p <= 0.4
        if _MODE == HYPTEST:
            sprt = SequentialProbabilityRatioTest(0.6, 0.7, 0.05, 0.05)
            accepted_hypothesis, results, info = world.run_repetitions(hypothesis_test=sprt)
            print("SPRT")
            print("Conducted tests: {}".format(len(results)))
            print("Successes: {} of {}".format(sum(results), len(results)))

            print("Hypothesis accepted: {}".format(accepted_hypothesis))
        elif _MODE == ESTIMATION:
            _, results, info = world.run_repetitions(number_of_repetitions=100)
            successes = sum(results)
            nobs = len(results)
            ratio = successes / nobs
            print("ESTIMATION")
            print("Successes: {} of {}".format(successes, nobs))
            print("Ratio: {}".format(ratio))
            alpha = 0.05

            print("Confidence intervals, alpha={}: ".format(alpha))
            for method in ["normal", "agresti_coull", "beta", "wilson", "jeffrey"]:
                ci_low, ci_upp = proportion.proportion_confint(successes, nobs, alpha=alpha, method=method)
                print("   {} => [{}..{}]".format(method, ci_low, ci_upp))







if __name__ == '__main__':
    unittest.main()
