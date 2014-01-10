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
import pyclp


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
            vehicle = Entity("vehicle" + str(i), "vehicle")
            world.addEntity(vehicle)

        mgen = MapGenerator()
        m = mgen.generate_map(10, 5, 15, 1000, 1000)

        mt = MapTranslator(m, world)
        mt.init_world_from_graph()

        vehicles = world.getDomain("vehicle")
        crossings = [n for n, data in m.nodes_iter(True) if data["loctype"] == "crossing"]
        pois = [n for n, data in m.nodes_iter(True) if data["loctype"] == "poi"]
        sams = world.getDomain("plcssam")
        plcses = world.getDomain("plcs")

        for sam in sams:
            world.setFluentValue("plcssam_vehicle_reservationRequests", [sam.id], [])
            world.setFluentValue("plcssam_vehicle_reservationResponses", [sam.id], [])

        for plcs in plcses:
            world.setConstantValue("maxCapacty", [plcs.id], 100)
            world.setFluentValue("plcsReservations", [plcs.id], [])
            world.setFluentValue("plcs_vehicle_reservationRequests", [plcs.id], [])

        for vehicle in vehicles:
            crossing = random.choice(crossings)
            target_poi = random.choice(pois)
            x, y = mt.get_position_from_node(target_poi)
            target = mt.find_closest_node(x, y, loctype="plcs")

            route = nx.shortest_path(m, crossing, target)

            world.setFluentValue("vehiclePosition", [vehicle.id], ("pos", crossing, crossing, 0))
            world.setFluentValue("vehicleSpeed", [vehicle.id], 10)
            world.setFluentValue("currentPLCS", [vehicle.id], "none")
            world.setFluentValue("currentTargetPLCS", [vehicle.id], target)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi)
            world.setFluentValue("currentRoute", [vehicle.id], route)
            world.setConstantValue("calendar", [vehicle.id], [("cal", target_poi, 1000, 2000)])

            # initialize defaults for ensembles
            for sam in sams:
                world.setFluentValue("vehicle_plcssam_reservationRequests", [vehicle.id, sam.id], [])
                world.setFluentValue("vehicle_plcssam_reservationResponses", [vehicle.id, sam.id], [])
                world.setFluentValue("ongoing_exchange_PLCSSAM_Vehicle", [vehicle.id, sam.id], False)

            for plcs in plcses:
                world.setFluentValue("vehicle_plcs_reservationRequests", [vehicle.id, plcs.id], [])
                world.setFluentValue("plcs_vehicle_reservationResponses", [plcs.id, vehicle.id], [])
                world.setFluentValue("ongoing_exchange_PLCS_Vehicle", [vehicle.id, plcs.id], False)

            world.setFluentValue("vehicle_plcs_reservationResponses", [vehicle.id], [])


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

        # vis = Visualizer()
        # plt.figure(1)
        # vis.visualize_map(m, world)
        #
        # World.logic_engine().progress([('tick',[])])
        # print("STEP 2:")
        # print("*" * 80)
        # world.printState()
        # # plt.figure(2)
        # vis.visualize_map(m, world)
        # plt.show()
        print(world.getFluentValue("vehiclePosition",["vehicle0"]))

        old_pos = pyclp.Compound("pos", pyclp.Atom("c1"), pyclp.Atom("c2"), 0)
        pos = pyclp.Var()
        route = pyclp.Var()
        #goal = pyclp.Compound("calculate_new_position2", pyclp.Atom("vehicle0"), old_pos, pos, pyclp.Atom("s0"))
        sit1 = pyclp.Compound("do2", pyclp.Atom("tick"), pyclp.Atom("s0"))
        # goal = pyclp.Compound("vehiclePosition", pyclp.Atom("vehicle0"), pos,
        #                       sit1)
        pyclp.Compound("currentRoute", pyclp.Atom("vehicle0"), route, pyclp.Atom("s0")).post_goal()
        pyclp.resume()
        pyclp.Compound("nextTarget2", pyclp.Atom("vehicle0"), pos, pyclp.Atom("s0")).post_goal()

        #pyclp.resume()
        print("Route: " + str(route.value()))
        print("Pos: " + str(pos.value()))



if __name__ == '__main__':
    unittest.main()
