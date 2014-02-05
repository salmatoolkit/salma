from numpy.distutils.system_info import agg2_info
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Procedure, Sequence, VariableAssignment, ActionExecution, Variable
from salma.model.process import TriggeredProcess
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

    return possible_target_chooser, route_finder









def create_plcssam_functions(world_map, mt):
    def process_requests_plcs(agent=None, plcssam_vehicle_reservationRequests=None, **ctx):
        #: :type: list of (str, str, list of str, int, int)
        requests = plcssam_vehicle_reservationRequests(agent.id)
        # format: rreq(Vehicle, Alternatives, StartTime, PlannedDuration)
        schedule = []
        assignment = dict()
        for r in requests:





class EMobilityScenario3(EMobilityTest):
    NUM_OF_VEHICLES = 3

    def create_vehicles(self, world, world_map, mt):
        target_chooser, route_finder, response_selector = create_navigation_functions(world_map, mt)

        p_request_plcs = Procedure("main", [],
                                   Sequence([
                                       VariableAssignment("possible_targets",
                                                          EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                          target_chooser, []),
                                       ActionExecution("queryPLCSSAM",
                                                       [Entity.SELF, "sam1", Variable("possible_targets"),
                                                        0, 0])
                                   ]))

        p_set_target = Procedure("main", [],
                                 Sequence([
                                     VariableAssignment("sam_response", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                        response_selector, []),
                                     ActionExecution("remove_all_vehicle_plcssam_reservationResponses",
                                                     [Entity.SELF, "sam1"]),
                                     ActionExecution("setTargetPLCS", [Entity.SELF, Variable("sam_response")])
                                 ]))

        p_find_route = Procedure("main", [],
                                 Sequence([
                                     VariableAssignment("route", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                        route_finder, []),
                                     ActionExecution("setRoute", [Entity.SELF, Variable("route")]),

                                 ]))

        for i in range(EMobilityScenario3.NUM_OF_VEHICLES):
            p1 = TriggeredProcess(p_request_plcs, EvaluationContext.PYTHON_EXPRESSION,
                                  "currentTargetPLCS(self) == 'none' and "
                                  "len(vehicle_plcssam_reservationRequests(self)) == 0", [])

            p2 = TriggeredProcess(p_find_route, EvaluationContext.PYTHON_EXPRESSION,
                                  "len(currentRoute(self)) == 0 and currentTargetPLCS(self) != 'none'", [])

            p3 = TriggeredProcess(p_set_target, EvaluationContext.PYTHON_EXPRESSION,
                                  "len(vehicle_plcssam_reservationResponses(self)) > 0", [])





            vehicle = Agent("v" + str(i), "vehicle", [p1, p2, p3])
            world.addAgent(vehicle)

    def create_plcssam(self):
        p_select_plcs = Procedure("main", [],
                                  Sequence([

                                  ])


    def test_scenario3(self):
        world = World.instance()

        mgen = MapGenerator(world)
        world_map = mgen.load_from_graphml("../../../testdata/test1.graphml")

        mt = MapTranslator(world_map, world)
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
        self.run_until_all_targets_reached(world, world_map, 200)


if __name__ == '__main__':
    unittest.main()
