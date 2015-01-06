import unittest
import logging
import os
from datetime import datetime

import matplotlib.pyplot as plt

from salma.model.evaluationcontext import EvaluationContext
from salma.model.experiment import Experiment
from salma.test.emobility.map_generator import MapGenerator
from salma.test.emobility.map_translator import MapTranslator
from salma.test.emobility.visualizer import Visualizer
from salma.model.world import World
from salma.engine import EclipseCLPEngine
from salma import SALMAException
from salma.model.core import Action


class EMobilityBase(Experiment):

    def __init__(self):
        super().__init__("ecl-test/e-mobility-with-abstractions/e-mobility-domain-with-abstractions.ecl")
        self.logger = logging.getLogger('salmalab')
        self.logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        self.logger.addHandler(ch)
        self.__world_map = None
        self.__mt = None
        self.__should_log = False
        self.__fig = None
        self.__visualizer = None
        self.__outdir = None
        self.__logfile = None

    @property
    def world_map(self):
        """
        The graph defining the map.
        :rtype: networkx.classes.graph.Graph
        """
        return self.__world_map

    @property
    def mt(self):
        """
        :rtype: MapTranslator
        """
        return self.__mt

    def create_entities(self):
        mgen = MapGenerator(self.world)
        self.__world_map = mgen.load_from_graphml("testdata/test1.graphml")
        # world_map = mgen.generate_map(5, 15, 25, 1000, 1000)
        self.__mt = MapTranslator(self.__world_map, self.world)
        self.mt.init_world_from_graph()

    def create_initial_situation(self):
        vehicles = self.world.getDomain("vehicle")
        crossings = [n for n, data in self.world_map.nodes_iter(True) if data["loctype"] == "crossing"]
        pois = [n for n, data in self.world_map.nodes_iter(True) if data["loctype"] == "poi"]
        sams = self.world.getDomain("plcssam")
        plcses = self.world.getDomain("plcs")

        for plcs in plcses:
            self.world.setFluentValue("plcsReservations", [plcs.id], [])
            self.world.setFluentValue("freeSlotsL", [plcs.id], None)

        for vehicle in vehicles:
            self.world.setFluentValue("currentPLCS", [vehicle.id], None)
            self.world.setFluentValue("currentTargetPLCS", [vehicle.id], None)
            self.world.setFluentValue("currentTargetPOI", [vehicle.id], None)
            self.world.setFluentValue("currentRoute", [vehicle.id], [])

    def __log(self, msg):
        print(msg)
        self.__logfile.write(msg + "\n")

    def record_step(self, world, verdict=None,
                    step=None, deltaT=None,
                    actions=None,
                    failedActions=None,
                    toplevel_results=None,
                    scheduled_results=None,
                    pending_properties=None):
        """
        :param World world: the world
        :param int step: the step number
        :param float deltaT: the duration
        :param list[Action] actions: the performed actions

        :param lis toplevel_results: the toplevel results
        :rtype: (bool, str)
        """
        if self.__should_log:
            time = world.getFluentValue("time", [])
            self.__log("Step {} (t = {})".format(step, time))
            self.__log("   Verdict: {}".format(verdict))
            self.__log("   Actions: {}".format(actions))

            messages = world.evaluation_context().evaluateFunction(EvaluationContext.ECLP_FUNCTION, "domain",
                                                                   "message")

            # for a in actions:
            #     if a[0] in ("requestTransfer", "transferStarts", "transferEnds", "transferFails"):
            #         msgid = a[1][0]
            for msgid in messages:
                    spec = world.evaluation_context().evaluateFunction(EvaluationContext.ECLP_FUNCTION, "message_spec",
                                                                       msgid)
                    stransval = world.getFluentValue("sensor_transmitted_value", [msgid])
                    chtransval = world.getFluentValue("channel_transmission_content", [msgid])
                    self.__log("      #{}: {}   sensor_transmitted_value: {}    "
                               "channel_trans_value: {}".format(msgid, spec, stransval, chtransval))

            self.__log("   Toplevel Results: {}".format(toplevel_results))
            self.__log("   Scheduled Results: {}".format(scheduled_results))

            vehicles = world.getDomain("vehicle")
            all_finished = True
            for vehicle in vehicles:
                pos = world.getFluentValue("vehiclePosition", [vehicle.id])
                route = world.getFluentValue("currentRoute", [vehicle.id])
                target = world.getFluentValue("currentTargetPLCS", [vehicle.id])
                speed = world.getFluentValue("vehicleSpeed", [vehicle.id])

                self.__log("   {}: {} - {} - {} - speed: {}".format(vehicle.id,
                                                        pos, route, target, speed))
            for plcs in world.getDomain("plcs"):
                fslocal = world.getFluentValue("freeSlotsL", [plcs.id])
                fsremote = world.getFluentValue("freeSlotsR", ["sam1", plcs.id])

                fsreal = world.evaluation_context().evaluateFunction(EvaluationContext.TRANSIENT_FLUENT, "freeSlots",
                                                                     plcs.id)
                self.__log("   {}: real = {}, local = {}, remote = {}".format(plcs, fsreal, fslocal, fsremote))

        if self.__visualizer is not None:
            image_file_name = "step_{:04}.png".format(step)
            path = os.path.join(self.__outdir, image_file_name)
            self.__fig.clf()
            self.__visualizer.visualize_map(self.__fig)
            # self.__fig.savefig(path, dpi=200)
            self.__fig.savefig(path)
        return True, None

    def start(self, step_limit=None, log=True, visualize=True):
        """
        Runs the simulation until all targets have been reached.

        :param step_limit:
        :param visualize:
        :return:
        """

        self.__should_log = log
        if visualize:
            self.__fig = plt.figure("emobility", (8, 8), 200)
            self.__visualizer = Visualizer(self.world_map, self.world)
        else:
            self.__fig = None
            self.__visualizer = None

        self.__logfile = None
        if self.__should_log:
            self.__outdir = datetime.now().strftime("imgout/%Y%m%d_%H-%M-%S")
            os.makedirs(self.__outdir)
            self.__logfile = open(os.path.join(self.__outdir, "log.txt"), mode="w")

        verdict, results = self.runExperiment(check_verdict=True, maxSteps=step_limit,
                                              stepListeners=[self.record_step])

        if self.__logfile is not None:
            self.__logfile.close()

        return verdict, results