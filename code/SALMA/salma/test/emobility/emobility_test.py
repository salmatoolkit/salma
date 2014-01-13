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


class EMobilityTest(unittest.TestCase):

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

    def init_map_and_defaults(self, world, m, mt):
        """
        :type world: World
        :type m: networkx.classes.graph.Graph
        :type mt: MapTranslator
        """
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
            world.setFluentValue("plcsReservations", [plcs.id], [])
            world.setFluentValue("plcs_vehicle_reservationRequests", [plcs.id], [])

        for vehicle in vehicles:
            world.setFluentValue("vehicleSpeed", [vehicle.id], 0)
            world.setFluentValue("currentPLCS", [vehicle.id], "none")
            world.setFluentValue("currentTargetPLCS", [vehicle.id], "none")
            world.setFluentValue("currentTargetPOI", [vehicle.id], "none")
            world.setFluentValue("currentRoute", [vehicle.id], [])

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

    def run_until_all_targets_reached(self, world, world_map, step_limit):
        vehicles = world.getDomain("vehicle")

        vis = Visualizer()
        i = 0

        all_finished = False
        while not all_finished and i < step_limit - 1:
            print("Step {}".format(i))
            all_finished = True
            for vehicle in vehicles:
                pos = world.getFluentValue("vehiclePosition", [vehicle.id])
                route =  world.getFluentValue("currentRoute", [vehicle.id])
                target = world.getFluentValue("currentTargetPLCS")
                print("{}: {} - {} - {}".format(vehicle.id,
                                           pos, route, target))
                if pos[1] != pos[2] or pos[1] != target:
                    all_finished = False

            path = "../../../imgout/step_{:04}.png".format(i)
            vis.visualize_map(world_map, world)
            plt.savefig(path)
            world.step()
            i += 1

if __name__ == '__main__':
    unittest.main()
