import unittest
import random

import networkx as nx

from salma.model.core import Entity
from salma.test.emobility.map_generator import MapGenerator
from salma.test.emobility.map_translator import MapTranslator
from salma.model.world import World
from salma.test.emobility.emobility_base import EMobilityBase


class EMobilityScenario2(EMobilityBase):
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
        starts = crossings.copy()
        pois = list(world.getDomain("poi"))
        target_pois = pois.copy()
        sams = world.getDomain("plcssam")
        plcses = world.getDomain("plcs")

        for plcs in plcses:
            world.setConstantValue("maxCapacty", [plcs.id], 10)

        targets = dict()
        for vehicle in vehicles:
            start = random.choice(starts)
            starts.remove(start)
            target_poi = random.choice(target_pois)
            target_pois.remove(target_poi)
            x, y = mt.get_position_from_node(target_poi.id)
            target = mt.find_closest_node(x, y, loctype="plcs")
            targets[vehicle.id] = target
            r = nx.shortest_path(world_map, start.id, target)

            world.setFluentValue("vehiclePosition", [vehicle.id], ("pos", start.id, start.id, 0))
            world.setFluentValue("vehicleSpeed", [vehicle.id], 10)
            world.setFluentValue("currentTargetPLCS", [vehicle.id], target)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi.id)
            world.setFluentValue("currentRoute", [vehicle.id], r)
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
        self.run_experiment(world, world_map, 200)





if __name__ == '__main__':
    unittest.main()
