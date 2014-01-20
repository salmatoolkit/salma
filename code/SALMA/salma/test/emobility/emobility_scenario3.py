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
from salma import SMCException
import logging
import salma
import os
import random
import matplotlib.pyplot as plt
import networkx as nx
import pyclp
from salma.test.emobility.emobility_test import EMobilityTest


def create_navigation_functions(world_map, mt):
    def target_chooser(agent=None, currentTargetPOI=None, **ctx):
        target_poi = currentTargetPOI(agent.id)
        x, y = mt.get_position_from_node(target_poi)
        target = mt.find_closest_node(x, y, loctype="plcs")
        return target

    def route_finder(agent=None, vehiclePosition=None, currentTargetPLCS=None, **ctx):
        pos = vehiclePosition(agent.id)
        target = currentTargetPLCS(agent.id)
        r = nx.shortest_path(world_map, pos[1], target)
        return r

    return target_chooser, route_finder


class EMobilityScenario3(EMobilityTest):
    NUM_OF_VEHICLES = 3

    def create_vehicles(self, world, world_map, mt):
        target_chooser, route_finder = create_navigation_functions(world_map, mt)

        p_set_target = Procedure("main", [],
                                 Sequence([
                                     VariableAssignment("target", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                        target_chooser, []),
                                     ActionExecution("setTargetPLCS", [Entity.SELF, Variable("target")])
                                 ]))
        p_find_route = Procedure("main", [],
                                 Sequence([
                                     VariableAssignment("route", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                                        route_finder, []),
                                     ActionExecution("setRoute", [Entity.SELF, Variable("route")])
                                 ]))

        for i in range(EMobilityScenario3.NUM_OF_VEHICLES):
            p1 = TriggeredProcess(p_set_target, EvaluationContext.PYTHON_EXPRESSION,
                                  "currentTargetPLCS(self) == 'none'", [])
            p2 = TriggeredProcess(p_find_route, EvaluationContext.PYTHON_EXPRESSION,
                                  "len(currentRoute(self)) == 0 and currentTargetPLCS(self) != 'none'", [])
            vehicle = Agent("v" + str(i), "vehicle", [p1, p2])
            world.addAgent(vehicle)



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
