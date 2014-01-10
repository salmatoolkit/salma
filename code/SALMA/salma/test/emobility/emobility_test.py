from cvxopt.misc import scale
from salma.model.core import Entity
from salma.test.emobility.map_generator import MapGenerator
from salma.test.emobility.map_translator import MapTranslator
from salma.test.emobility.visualizer import Visualizer

__author__ = 'kroiss'

import unittest
from salma.model.world import World
from salma.engine import EclipseCLPEngine
from salma import SMCException
import logging
import salma
import os
import random
from salma.model.distributions import BernoulliDistribution
import matplotlib.pyplot as plt
import networkx as nx


class EMobilityTest(unittest.TestCase):

    NUM_OF_VEHICLES = 3

    @classmethod
    def setUpClass(cls):

        try:
            World.set_logic_engine(EclipseCLPEngine("../../../ecl-test/e-mobility/e-mobility-domain.ecl"))
        except SMCException as e:
            print(e)
            raise
        logger = logging.getLogger('salma')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)

    def setUp(self):
        self.logger = logging.getLogger('salma')
        World.create_new_world()
        world = World.instance()
        world.load_declarations()

    def testWorldCreation(self):
        world = World.instance()

        for i in range(EMobilityTest.NUM_OF_VEHICLES):
            vehicle = Entity("vehice" + str(i), "vehicle")
            world.addEntity(vehicle)

        mgen = MapGenerator()
        m = mgen.generate_map(10, 5, 15, 1000, 1000)

        mt = MapTranslator(m, world)
        mt.init_world_from_graph()

        vehicles = world.getDomain("vehicle")
        crossings = [n for n, data in m.nodes_iter(True) if data["loctype"] == "crossing"]
        pois = [n for n, data in m.nodes_iter(True) if data["loctype"] == "poi"]


        for vehicle in vehicles:
            crossing = random.choice(crossings)
            target_poi = random.choice(pois)
            x, y = mt.get_position_from_node(target_poi)
            target = mt.find_closest_node(x, y, loctype="plcs")

            route = nx.shortest_path(m, crossing, target)

            world.setFluentValue("vehiclePosition", [vehicle.id], ("pos", crossing, crossing, 0))
            world.setFluentValue("vehicleSpeed", [vehicle.id], 0)
            world.setFluentValue("currentPLCS", [vehicle.id], "none")
            world.setFluentValue("currentTargetPLCS", [vehicle.id], target)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi)
            world.setFluentValue("currentRoute", [vehicle.id], route)
            world.setConstantValue("calendar", [vehicle.id], [("cal", target_poi, 1000, 2000)])

        plcs = world.getDomain("plcs")

        for p in plcs:
            world.setConstantValue("maxCapacty", [p.id], 100)
            world.setFluentValue("plcsReservations", [p.id], [])



        print(world.getSorts())

        locs = world.getDomain("location")
        for l in locs:
            x = world.getConstantValue("locX", [l.id])
            y = world.getConstantValue("locY", [l.id])
            print("{}: x={}, y={}".format(l, x, y))


        print("-" * 80)
        world.printState()
        uninitialized_fluent_instances, uninitialized_constant_instances = world.check_fluent_initialization()
        print("-" * 80)
        print("Uninitialized Fluents:")
        print(uninitialized_fluent_instances)
        print("-" * 80)
        print("Uninitialized Constants:")
        print(uninitialized_constant_instances)


        vis = Visualizer()
        vis.visualize_map(m)



if __name__ == '__main__':
    unittest.main()
