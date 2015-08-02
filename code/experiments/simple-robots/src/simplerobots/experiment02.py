from io import TextIOBase
import logging
from logging import FileHandler
import logging.config
from math import sqrt
from numpy import random
from salma.constants import OK, NOT_OK
from salma.experiment import Experiment, SingleProcessExperimentRunner
from salma.model.core import Entity, translate_entities
from salma.model.distributions import ConstantDistribution, Never, BernoulliDistribution, NEVER, \
    GeometricDistribution, \
    CustomDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.selectionstrategy import NonDeterministic, Categorical
from salma.model.world import World
from src.simplerobots.agents import create_robot, create_coordinator, create_coordinator_clever
import numpy as np
import json
from pathlib import Path
from datetime import datetime

NUM_ROBOTS = 20
NUM_ITEMS = 100
NUM_STATIONS = 10
GRID_WIDTH = 500
GRID_HEIGHT = 500
N_SLOTS = 5
P_SLOT = 0.01
COLLISION_PROB = 1.0
# SEED = int(datetime.now().timestamp())
SEED = 1438470243


def break_when_all_delivered(world: World, **kwargs):
    for i in world.getDomain("item"):
        if i.delivered_to is None:
            return None
    return OK


def break_when_all_broken(world: World, **kwargs):
    for r in world.getDomain("robot"):
        if r.broken is False:
            return None
    return NOT_OK


class Experiment02(Experiment):
    def __init__(self, num_exp, num_robots, clever,
                 expfile: TextIOBase, initpath: Path=None):
        super().__init__("ecl-src/simple-robots-domaindesc.ecl")
        self.num_exp = num_exp
        self.num_robots = num_robots
        self.clever = clever
        self.expfile = expfile
        self.initpath = initpath

    def initialize(self):
        np.random.seed(SEED)
        random.seed(SEED)
        super().initialize()

    def load_initial_situation(self):
        with self.initpath.open() as f:
            config = json.load(f)
        coordinator1, = self.world.get_entities_by_id("coordinator1")
        robots = self.world.getDomain("robot")
        items = self.world.getDomain("item")
        workstations = self.world.getDomain("workstation")
        for r in robots:

            r.xpos = config["robots"][r.id]["x"]
            r.ypos = config["robots"][r.id]["y"]
            r.vx = 0
            r.vy = 0
            r.broken = False
            r.next_task = None
            r.robot_radius = 1
            for i in items:
                r.set_carrying(i, False)

        coordinator1.request_queue = []

        for item in items:
            item.xpos = config["items"][item.id]["x"]
            item.ypos = config["items"][item.id]["y"]
            item.delivered_to = None

        for ws in workstations:
            ws.stationX = config["workstations"][ws.id]["x"]
            ws.stationY = config["workstations"][ws.id]["y"]
            ws.delivered_item_count = 0

    def create_initial_situation(self):
        if self.initpath is None:
            self.create_random_initial_situation()
        else:
            self.load_initial_situation()

    def create_random_initial_situation(self):
        coordinator1, = self.world.get_entities_by_id("coordinator1")
        robots = self.world.getDomain("robot")
        items = self.world.getDomain("item")
        workstations = self.world.getDomain("workstation")

        for r in robots:
            r.xpos = np.random.randint(1, GRID_WIDTH)
            r.ypos = np.random.randint(1, GRID_HEIGHT)
            r.vx = 0
            r.vy = 0
            r.broken = False
            r.next_task = None
            r.robot_radius = 1
            for i in items:
                r.set_carrying(i, False)

        coordinator1.request_queue = []

        for item in items:
            item.xpos = np.random.randint(1, GRID_WIDTH)
            item.ypos = np.random.randint(1, GRID_HEIGHT)
            item.delivered_to = None

        for ws in workstations:
            ws.stationX = np.random.randint(1, GRID_WIDTH)
            ws.stationY = np.random.randint(1, GRID_HEIGHT)
            ws.delivered_item_count = 0

    def create_entities(self):

        if self.clever:
            coordinator1 = create_coordinator_clever()
        else:
            coordinator1 = create_coordinator()

        self.world.add(coordinator1)
        for r in range(1, self.num_robots + 1):
            self.world.add(create_robot(r))
        for i in range(1, NUM_ITEMS + 1):
            self.world.add(Entity("item" + str(i), "item"))
        for i in range(1, NUM_STATIONS + 1):
            self.world.add(Entity("ws" + str(i), "workstation"))

    def setup_distributions(self):
        world = self.world
        world.deactivate_info_transfer()

        step_finished = world.get_exogenous_action_choice("step_finished")
        step_finished.selection_strategy = Categorical(step_succeeded=0.8, step_failed=0.2)
        stepdelay = ConstantDistribution("integer", 1)
        world.get_exogenous_action("step_succeeded").config.occurrence_distribution = \
            stepdelay
        world.get_exogenous_action("step_failed").config.occurrence_distribution = \
            stepdelay

        pickup = world.get_stochastic_action("pickUp")
        grab = pickup.outcome("grab")
        grab.map_param("r", "r"), grab.map_param("i", "i")
        grab.uniform_param("grip", value_range=(1, 10))
        drop = pickup.outcome("drop")
        drop.map_param("r", "r"), drop.map_param("i", "i")
        pickup.selection_strategy = Categorical(grab=0.7, drop=0.3)

        accidental_drop = world.get_exogenous_action("accidental_drop")

        accidental_drop.config.occurrence_distribution = GeometricDistribution(0.001)

        collision_event = world.get_exogenous_action("collision")
        collision_event.config.occurrence_distribution = BernoulliDistribution(COLLISION_PROB)

        collision_event.config.uniform_param("severity", value_range=(1, 10))

        request_event = world.get_exogenous_action("request")

        def request_distrib(ws, c, ctx: EvaluationContext=None, **kwargs):
            ws_in_queue = c.request_queue.count(ws)
            assigned_robots = len([r for r in ctx.getDomain("robot") if r.task_workstation == ws])
            n_free = N_SLOTS - assigned_robots - ws_in_queue
            p_tot = 1 - (1 - P_SLOT) ** n_free
            return None if p_tot == 0 else np.random.geometric(p_tot)

        request_event.config.occurrence_distribution = CustomDistribution("integer",
                                                                          request_distrib)

    def after_run(self, verdict, **kwargs):
        columns = [self.num_exp, self.num_robots, self.clever]
        num_broken = 0
        for r in self.world.getDomain("robot"):
            if r.broken:
                num_broken += 1
        columns.append(num_broken)
        wscountsum = 0
        for ws in sorted(self.world.getDomain("workstation")):
            c = ws.delivered_item_count
            columns.append(c)
            wscountsum += c
        columns.append(wscountsum)
        self.expfile.write(";".join(list(map(str, columns))) + "\n")
        self.expfile.flush()
        logger = logging.getLogger(MODULE_LOGGER_NAME)
        logger.info("DONE!    broken: {:3}, delivered: {:3}".format(num_broken, wscountsum))


def create_csv_header():
    columns = ["expnum", "num_robots", "clever", "num_broken"]
    for i in range(1, NUM_STATIONS + 1):
        columns.append("wscount" + str(i))
    columns.append("wscountsum")
    return ";".join(columns)


if __name__ == '__main__':
    basepath = Path("experiment_results")
    now = datetime.now()
    timestamp = now.strftime("%Y_%m_%d-%H_%M_%S")
    experiment_path = basepath.joinpath("exp02-" + timestamp)
    num = 2
    while experiment_path.exists():
        experiment_path = basepath.joinpath("exp02-" + timestamp + "_" + "v" + str(num))
    experiment_path.mkdir()

    MODULE_LOGGER_NAME = 'salma'
    # logging.config.fileConfig("experiment01.logging.conf")
    logging.basicConfig()
    logger = logging.getLogger(MODULE_LOGGER_NAME)
    logger.setLevel(logging.INFO)

    # experiment = Experiment02(experiment_path, Path("config_3r_20i_5s_200x200.json"))
    # experiment = Experiment02(experiment_path)

    num_exp = 1
    runner = SingleProcessExperimentRunner()

    with experiment_path.joinpath("experiment.csv").open("w") as f:
        f.write(create_csv_header() + "\n")
        for num_robots in range(45, 85, 5):
            for clever in [False, True]:
                for i in range(10):
                    if experiment_path.joinpath("stop.txt").exists():
                        break
                    else:
                        experiment = Experiment02(num_exp, num_robots, clever, f)
                        experiment.initialize()
                        experiment.step_listeners.append(break_when_all_delivered)
                        experiment.step_listeners.append(break_when_all_broken)
                        logger.info(
                            "Experiment #{:3} --  robots: {:3},  "
                            "clever: {:>5} \t trial {:4}".format(
                                num_exp, num_robots, str(clever), i))
                        experiment.run(max_steps=500)
                        num_exp += 1
