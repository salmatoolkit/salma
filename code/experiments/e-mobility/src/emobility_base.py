from collections.abc import Iterable
import logging
import os
from datetime import datetime

import matplotlib.pyplot as plt
import numpy as np

from salma.model.data import Term
from salma.model.evaluationcontext import EvaluationContext
from salma.experiment import Experiment
from map_generator import MapGenerator
from map_translator import MapTranslator
from visualizer import Visualizer
from salma.model.world import World
from salma.model.core import Action

ASPECT_RATIO = 1.4
FIG_HEIGHT = 8


def print_timing_info(info):
    steps = [ti["steps"] for ti in info]

    times = [ti["time"].total_seconds() for ti in info]
    print(
        "Steps: mean = {}, median = {}, min={}, max={}".format(np.mean(steps), np.median(steps), np.min(steps),
                                                               np.max(steps)))
    print("Time: mean = {}, median = {}, min={}, max={}".format(np.mean(times), np.median(times), np.min(times),
                                                                np.max(times)))


def get_road_info(world, *road_ids):
    """
    :param World world: the world
    :param list[str] road_id: the road id
    :rtype: list[str]
    """
    result = []
    for r in road_ids:
        road_ends = world.getConstantValue("roadEnds", [r])
        road_length = world.getConstantValue("roadlength", [r])
        assert isinstance(road_ends, Term)
        result.append("{}({}-{}={})".format(r, road_ends.params[0], road_ends.params[1], road_length))
    return result


class EMobilityBase(Experiment):
    def __init__(self, should_log, should_visualize):
        """
        :param bool should_log: whether or not logging should be activated.
        :param bool should_visualize: whether or not the simulation should be visulaized.
        """
        super().__init__("ecl-src/e-mobility-domain-with-abstractions.ecl")
        self.logger = logging.getLogger('salmalab')
        self.logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        self.logger.addHandler(ch)
        #: :type : networkx.classes.graph.Graph
        self.__world_map = None
        #: :type : MapTranslator
        self.__mt = None
        self.__should_log = should_log
        self.__should_visualize = should_visualize
        #: :type : Figure
        self.__fig = None
        self.__visualizer = None
        #: :type : str
        self.__outdir = None
        #: :type : IOBase
        self.__logfile = None
        if self.__should_log or self.__should_visualize:
            self.step_listeners.append(self.record_step)

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

            messages = world.evaluation_context.evaluateFunction(
                EvaluationContext.ECLP_FUNCTION, "domain",
                "message")

            # for a in actions:
            # if a[0] in ("requestTransfer", "transferStarts", "transferEnds", "transferFails"):
            # msgid = a[1][0]
            for msgid in messages:
                spec = world.evaluation_context.evaluateFunction(EvaluationContext.ECLP_FUNCTION, "message_spec",
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
                assert isinstance(pos, Term)
                if pos.functor == "r":
                    pos2 = get_road_info(world, pos.params[0])[0]
                else:
                    pos2 = pos.params[0]

                route = world.getFluentValue("currentRoute", [vehicle.id])
                assert isinstance(route, Iterable)
                route2 = get_road_info(world, *route)
                target = world.getFluentValue("currentTargetPLCS", [vehicle.id])
                currentPLCS = world.getFluentValue("currentPLCS", [vehicle.id])
                self.__log("   {}: {} - {} - {} / {}".format(vehicle.id,
                                                             pos2, route2, target, currentPLCS))
            for plcs in world.getDomain("plcs"):
                fslocal = world.getFluentValue("freeSlotsL", [plcs.id])
                fsremote = world.getFluentValue("freeSlotsR", ["sam1", plcs.id])
                fsreal = world.get_derived_fluent_value("freeSlots", [plcs.id])
                self.__log("   {}: real = {}, local = {}, remote = {}".format(plcs, fsreal, fslocal, fsremote))

        if self.__visualizer is not None:
            # image_file_name = "step_{:04}.png".format(step)
            image_file_name = "step_{:04}.pdf".format(step)
            path = os.path.join(self.__outdir, image_file_name)
            self.__fig.clf()
            self.__visualizer.visualize_map(self.__fig)
            self.__fig.savefig(path, dpi=200)
            # self.__fig.savefig(path)
        return True, None

    def before_run(self, **kwargs):
        if self.__should_visualize:

            self.__fig = plt.figure("emobility", (ASPECT_RATIO * FIG_HEIGHT, FIG_HEIGHT), 200)
            self.__visualizer = Visualizer(self.world_map, self.world)
        else:
            self.__fig = None
            self.__visualizer = None

        self.__logfile = None
        if self.__should_log:
            self.__outdir = datetime.now().strftime("imgout/%Y%m%d_%H-%M-%S")
            os.makedirs(self.__outdir)
            self.__logfile = open(os.path.join(self.__outdir, "log.txt"), mode="w")

    def after_run(self, verdict, **kwargs):
        if self.__logfile is not None:
            self.__logfile.close()





