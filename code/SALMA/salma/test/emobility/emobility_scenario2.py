from salma.model.core import Entity
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


class EMobilityScenario2(EMobilityTest):
    NUM_OF_VEHICLES = 3

    def createVehicles(self, world):
         for i in range(EMobilityScenario2.NUM_OF_VEHICLES):
            vehicle = Entity("v" + str(i), "vehicle")
            world.addEntity(vehicle)

    def test_scenario1(self):
        world = World.instance()
        self.createVehicles(world)

        mgen = MapGenerator(world)
        world_map = mgen.load_from_graphml("../../../testdata/test1.graphml")


        mt = MapTranslator(world_map, world)
        self.init_map_and_defaults(world, world_map, mt)

        vehicles = world.getDomain("vehicle")
        crossings = list(world.getDomain("crossing"))
        pois = list(world.getDomain("poi"))
        starts = pois.copy()
        sams = world.getDomain("plcssam")
        plcses = world.getDomain("plcs")

        for plcs in plcses:
            world.setConstantValue("maxCapacty", [plcs.id], 10)

        targets = dict()
        for vehicle in vehicles:
            crossing = random.choice(crossings).id
            target_poi = random.choice(starts)
            starts.remove(target_poi)
            x, y = mt.get_position_from_node(target_poi.id)
            target = mt.find_closest_node(x, y, loctype="plcs")
            targets[vehicle.id] = target
            r = nx.shortest_path(world_map, crossing, target)

            world.setFluentValue("vehiclePosition", [vehicle.id], ("pos", crossing, crossing, 0))
            world.setFluentValue("vehicleSpeed", [vehicle.id], 10)
            world.setFluentValue("currentTargetPLCS", [vehicle.id], target)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi)
            world.setFluentValue("currentRoute", [vehicle.id], r)
            world.setConstantValue("calendar", [vehicle.id], [("cal", target_poi, 100, 100)])

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

        self.run_until_all_targets_reached(world, world_map, 200)





if __name__ == '__main__':
    unittest.main()
