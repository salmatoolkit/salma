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
        targets = dict()
        for vehicle in vehicles:
            crossing = random.choice(crossings)
            target_poi = random.choice(pois)
            x, y = mt.get_position_from_node(target_poi)
            target = mt.find_closest_node(x, y, loctype="plcs")
            targets[vehicle.id] = target
            r = nx.shortest_path(m, crossing, target)
            # crossing = "c1"
            # target_poi = "poi1"
            # target = "plcs1"
            # r = ["c1", "c2", "plcs1"]
            world.setFluentValue("vehiclePosition", [vehicle.id], ("pos", crossing, crossing, 0))
            world.setFluentValue("vehicleSpeed", [vehicle.id], 10)
            world.setFluentValue("currentPLCS", [vehicle.id], "none")
            world.setFluentValue("currentTargetPLCS", [vehicle.id], target)
            world.setFluentValue("currentTargetPOI", [vehicle.id], target_poi)
            world.setFluentValue("currentRoute", [vehicle.id], r)
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

        vis = Visualizer()
        i = 0
        all_reached_target = False
        while not all_reached_target:
            print("Step {}".format(i))
            all_reached_target = True
            for vehicle in vehicles:
                pos = world.getFluentValue("vehiclePosition", [vehicle.id])
                print("{}: {} - {}".format(vehicle.id,
                                           pos,
                                           world.getFluentValue("currentRoute", [vehicle.id])))
                if pos[1] != pos[2] or pos[1] != targets[vehicle.id]:
                    all_reached_target = False

            path = "../../../imgout/step_{:02}.png".format(i)
            vis.visualize_map(m, world)
            plt.savefig(path)
            World.logic_engine().progress([('tick',[])])
            i += 1


if __name__ == '__main__':
    unittest.main()
