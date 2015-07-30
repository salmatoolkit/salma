import unittest
import random

import networkx as nx

from salma.model.core import Entity
from salma.test.emobility.map_generator import MapGenerator
from salma.test.emobility.map_translator import MapTranslator
from salma.model.world import World
from salma.test.emobility.emobility_base import EMobilityBase


class EMobilityScenario1(EMobilityBase):
    NUM_OF_VEHICLES = 3

    def createVehicles(self, world):
         for i in range(EMobilityScenario1.NUM_OF_VEHICLES):
            vehicle = Entity("vehicle" + str(i), "vehicle")
            world.addEntity(vehicle)

    def test_scenario1(self):
        world = World.instance()
        self.createVehicles(world)
        mgen = MapGenerator(world)
        world_map = mgen.generate_map(5, 10, 10, 500, 500)
        mt = MapTranslator(world_map, world)
        self.init_map_and_defaults(world, world_map, mt)

        vehicles = world.getDomain("vehicle")
        crossings = list(world.getDomain("crossing"))
        pois = list(world.getDomain("poi"))
        sams = world.getDomain("plcssam")
        plcses = world.getDomain("plcs")

        for plcs in plcses:
            world.set_constant_value("maxCapacty", [plcs.id], 10)

        targets = dict()
        for vehicle in vehicles:
            crossing = random.choice(crossings).id
            target_poi = random.choice(pois)
            x, y = mt.get_position_from_node(target_poi.id)
            target = mt.find_closest_node(x, y, loctype="plcs")
            targets[vehicle.id] = target
            r = nx.shortest_path(world_map, crossing, target)

            world.set_fluent_value("vehiclePosition", [vehicle.id], ("pos", crossing, crossing, 0))
            world.set_fluent_value("vehicleSpeed", [vehicle.id], 10)
            world.set_fluent_value("currentTargetPLCS", [vehicle.id], target)
            world.set_fluent_value("currentTargetPOI", [vehicle.id], target_poi)
            world.set_fluent_value("currentRoute", [vehicle.id], r)
            world.set_constant_value("calendar", [vehicle.id], [("cal", target_poi, 100, 100)])

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

        self.run_experiment(world, world_map, 200)





if __name__ == '__main__':
    unittest.main()
