from numpy.distutils.system_info import agg2_info
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import BernoulliDistribution, DelayedOccurrenceDistribution, NormalDistribution, \
    ConstantDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Procedure, Sequence, Assign, Act, Variable, Iterate, Send, Receive, SetFluent
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
from salma.model.infotransfer import ReceivedMessage

HYPTEST, ESTIMATION, VISUALIZE = range(3)

_MODE = VISUALIZE


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

    def response_selector(agent=None, sam_responses=None, **ctx):
        """
        :type sam_responses: list[ReceivedMessage]
        :rtype: str
        """
        # for now: ignore any time information
        # format: rresp(StartTime, PlannedDuration, BestPLCS)
        if len(sam_responses) > 0:
            return sam_responses[0].content[3]
        else:
            return None

    return possible_target_chooser, route_finder, response_selector


def create_plcssam_functions(world_map, mt):
    def process_assignment_requests(agent=None, assignment_requests=None, **ctx):
        """
        :type agent: salma.model.agent.Agent
        :type assignment_requests: list[ReceivedMessage]
        :rtype: list[(str,str)]
        """
        # : :type : EvaluationContext
        ec = agent.evaluation_context

        # format: rreq(Vehicle, Alternatives, StartTime, PlannedDuration)
        schedule = []
        assignment = dict()
        for r in assignment_requests:
            schedule.append((r.content[1], r.content[2]))  # remember that position 0 is the "message envelope" "rreq"
        success = utils.choose_alternative(schedule, assignment)
        if not success:
            return []
        result = []
        for vehicle, plcs in assignment.items():
            result.append((vehicle, plcs))

        # todo: establish communication between SAM and PLCs to check availability
        # TODO: consider time slots
        return result

    return process_assignment_requests


class EMobilityScenario1(EMobilityTest):
    NUM_OF_VEHICLES = 3

    def create_vehicles(self, world, world_map, mt):
        target_chooser, route_finder, response_selector = create_navigation_functions(world_map, mt)

        p_request_plcs = Procedure("main", [],
                                   [
                                       Assign("possible_targets",
                                              EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                              target_chooser, []),
                                       Send("assignment", "veh", "sam1", "sam",
                                            ("areq", Entity.SELF, Variable("possible_targets"), 0, 0)),
                                       SetFluent("waitingForAssignment", EvaluationContext.PYTHON_EXPRESSION, "True",
                                                 [Entity.SELF])
                                   ])

        p_set_target = Procedure("main", [],
                                 [
                                     Receive("assignment", "veh", "sam_responses"),
                                     Assign("chosen_plcs", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                            response_selector, []),
                                     Act("setTargetPLCS", [Entity.SELF, Variable("chosen_plcs")])
                                 ])

        p_find_route = Procedure("main", [],
                                 [
                                     Assign("route", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                            route_finder, []),
                                     Act("setRoute", [Entity.SELF, Variable("route")])
                                 ])

        for i in range(EMobilityScenario1.NUM_OF_VEHICLES):
            p1 = TriggeredProcess(p_request_plcs, EvaluationContext.PYTHON_EXPRESSION,
                                  "currentTargetPLCS(self) == 'none' and "
                                  "not waitingForAssignment(self)", [])
            # TODO: handle time-out for response from SAM

            p2 = TriggeredProcess(p_find_route, EvaluationContext.PYTHON_EXPRESSION,
                                  "len(currentRoute(self)) == 0 and currentTargetPLCS(self) != 'none'", [])

            p3 = TriggeredProcess(p_set_target, EvaluationContext.PYTHON_EXPRESSION,
                                  "len(local_channel_in_queue(self, 'assignment', 'veh')) > 0", [])

            vehicle = Agent("v" + str(i), "vehicle", [p1, p2, p3])
            world.addAgent(vehicle)

    def create_plcssam(self, world, world_map, mt):
        request_processor = create_plcssam_functions(world_map, mt)

        p_process_requests = Procedure("main", [],
                                       [

                                           Receive("assignment", "sam", "assignment_requests"),
                                           Assign("assignments", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                  request_processor, []),
                                           Iterate(EvaluationContext.ITERATOR, Variable("assignments"),
                                                   [("v", "vehicle"), ("p", "plcs")],
                                                   Send("assignment", "sam", Variable("v"), "veh",
                                                        ("aresp", 0, 0, Variable("p"))))
                                       ])

        p1 = TriggeredProcess(p_process_requests, EvaluationContext.PYTHON_EXPRESSION,
                              "len(local_channel_in_queue(self, 'assignment', 'sam')) > 0", [])

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
        # world_map = mgen.generate_map(5, 15, 25, 1000, 1000)
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
            # target_pois.remove(target_poi)

            world.setFluentValue("vehiclePosition", [vehicle.id], ("pos", start.id, start.id, 0))
            world.setFluentValue("vehicleSpeed", [vehicle.id], 10)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi.id)
            world.setConstantValue("calendar", [vehicle.id], [("cal", target_poi.id, 100, 100)])

        transferStarts = world.get_exogenous_action("transferStarts")
        transferStarts.config.occurrence_distribution = DelayedOccurrenceDistribution(NormalDistribution("float", 5, 1))
        transferStarts.config.set_param_distribution("error", ConstantDistribution("term", None))


        transferEnds = world.get_exogenous_action("transferEnds")
        transferEnds.config.occurrence_distribution = DelayedOccurrenceDistribution(NormalDistribution("float", 10, 1))
        transferEnds.config.set_param_distribution("error", ConstantDistribution("term", None))

        transferFails = world.get_exogenous_action("transferFails")
        transferFails.config.set_probability(0.01)

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
        if _MODE != VISUALIZE:
            world.registerProperty("f", fstr, World.INVARIANT)

        world.registerProperty("g", gstr, World.ACHIEVE)
        if log:
            self.__print_info(world)

        # verdict, results = self.run_experiment(world, world_map, log=False, visualize=False)
        # print("Verdict: {}\nResults:\n{}".format(verdict, results))
        # results, infos = world.run_repetitions(100)

        # assumption success prob = 0.6 --> H0: p <= 0.4
        if _MODE == HYPTEST:
            sprt = SequentialProbabilityRatioTest(0.6, 0.7, 0.01, 0.01)
            accepted_hypothesis, results, info = world.run_repetitions(hypothesis_test=sprt)
            print("SPRT")
            print("Conducted tests: {}".format(len(results)))
            print("Successes: {} of {}".format(sum(results), len(results)))

            print("Hypothesis accepted: {}".format(accepted_hypothesis))
        elif _MODE == ESTIMATION:
            _, results, info = world.run_repetitions(number_of_repetitions=20)
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
        elif _MODE == VISUALIZE:
            print("Visualize")
            verdict, _ = self.run_experiment(world, world_map, log=True, visualize=True)
            print("Verdict: {}".format(verdict))


if __name__ == '__main__':
    unittest.main()
