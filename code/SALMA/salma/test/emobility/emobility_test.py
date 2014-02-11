from statsmodels.base.tests.test_shrink_pickle import RemoveDataPickle
from salma.model.core import Entity
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
from datetime import datetime


class EMobilityTest(unittest.TestCase):
    POSTER_PREFIX = r"""
\documentclass[a4paper]{article}
\usepackage[top=1cm, bottom=1.5cm, left=.5cm, right=.5cm]{geometry}
\usepackage{graphicx}
\begin{document}
\begin{tabular}{cccc}
"""

    POSTER_SUFFIX = r"""
\end{tabular}
\end{document}
"""

    @classmethod
    def setUpClass(cls):

        try:
            World.set_logic_engine(EclipseCLPEngine("../../../ecl-test/e-mobility/e-mobility-domain.ecl"))
        except SALMAException as e:
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

    def __log(self, msg):
        print(msg)
        self.__logfile.write(msg + "\n")

    def record_step(self, world, step, deltaT=None, actions=None, toplevel_results=None, scheduled_results=None):
        """

        :param World world: the world
        :param int step: the step number
        :param float deltaT: the duration
        :param list[Action] actions: the performed actions
        :param lis toplevel_results: the toplevel results
        :rtype: (bool, str)
        """
        self.__log("Step {}".format(step))
        self.__log("   Actions: {}".format(actions))
        self.__log("   Toplevel Results: {}".format(toplevel_results))
        self.__log("   Scheduled Results: {}".format(scheduled_results))
        vehicles = world.getDomain("vehicle")
        all_finished = True
        for vehicle in vehicles:
            pos = world.getFluentValue("vehiclePosition", [vehicle.id])
            route = world.getFluentValue("currentRoute", [vehicle.id])
            target = world.getFluentValue("currentTargetPLCS", [vehicle.id])
            self.__log("{}: {} - {} - {}".format(vehicle.id,
                                                 pos, route, target))
            if target == "none" or pos[1] != pos[2] or pos[1] != target:
                all_finished = False

        #imagefiles.append(imagefilename)

        if self.__visualizer is not None:
            imagefilename = "step_{:04}.png".format(step)
            path = os.path.join(self.__outdir, imagefilename)
            self.__fig.clf()
            self.__visualizer.visualize_map(self.__fig)
            self.__fig.savefig(path, dpi=200)
        if all_finished:
            return False, "All vehicles reached target."
        else:
            return True, None

    def run_until_all_targets_reached(self, world, world_map, step_limit=None, visualize=True):
        """
        Runs the simulation until all targets have been reached.

        :param World world: the world
        :param world_map:
        :param step_limit:
        :param visualize:
        :return:
        """


        fstr = """
        forall([v,vehicle],
                implies(
                    occur(queryPLCSSAM(v,?,?,?,?)),
                    until(5,
                        true,
                        hasTargetPLCS(v)
                    )
                )
        )
        """
        world.registerProperty("f",fstr)
        if visualize:
            self.__fig = plt.figure("emobility")
            self.__visualizer = Visualizer(world_map, world)
        else:
            self.__fig = None
            self.__visualizer = None
        self.__outdir = datetime.now().strftime("../../../imgout/%Y%m%d_%H-%M-%S")
        os.makedirs(self.__outdir)
        with open(os.path.join(self.__outdir, "log.txt"), mode="w") as f:
            self.__logfile = f
            # world.runUntilFinished(maxSteps=step_limit, stepListeners=[self.record_step])
            world.runExperiment(check_verdict=True, maxSteps=step_limit, stepListeners=[self.record_step])


            # with open(os.path.join(outdir, "poster.tex"), mode="w") as f:
            #     f.write(EMobilityTest.POSTER_PREFIX)
            #     for i, filename in enumerate(imagefiles):
            #         f.write("\\includegraphics[width=4cm]{%s}\n" % filename)
            #         if i % 4 == 0:
            #             f.write("\\\\\n")
            #         else:
            #             f.write("&\n")
            #     for i in range(4 - (len(imagefiles) % 4)):
            #         f.write("&\n")
            #     f.write(EMobilityTest.POSTER_SUFFIX)


if __name__ == '__main__':
    unittest.main()
