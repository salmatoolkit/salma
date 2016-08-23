import argparse
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
from simplerobots.agents import create_robot, create_coordinator, create_coordinator_clever
import numpy as np
import json
from pathlib import Path
from datetime import datetime
from datetime import datetime, timedelta



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
                 expfilepath: Path, initpath: Path=None):
        super().__init__("ecl-src/simple-robots-domaindesc.ecl")
        self.num_exp = num_exp
        self.num_robots = num_robots
        self.clever = clever
        self.expfile = expfilepath
        self.initpath = initpath
        self.num_collisions = 0
        self.step_listeners.append(self.__count_collision)

    def initialize(self):
        np.random.seed(SEED)
        random.seed(SEED)
        self.num_collisions = 0
        super().initialize()

    def __count_collision(self, world: World, actions=None, **kwargs):
        if actions is not None:
            for a in actions:
                if a[0] == "collision":
                    self.num_collisions += 1

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

    def after_run(self, verdict, time: timedelta=None, **kwargs):
        columns = [self.num_exp, time.total_seconds(), self.num_robots, self.clever]
        num_broken = 0
        for r in self.world.getDomain("robot"):
            if r.broken:
                num_broken += 1
        columns.append(self.num_collisions)
        columns.append(num_broken)
        wscountsum = 0
        for ws in sorted(self.world.getDomain("workstation")):
            c = ws.delivered_item_count
            columns.append(c)
            wscountsum += c
        columns.append(wscountsum)
        with self.expfile.open(mode="a") as f:
            f.write(";".join(list(map(str, columns))) + "\n")
            f.flush()

        logger = logging.getLogger(MODULE_LOGGER_NAME)
        logger.info(
            "DONE ({:.4}!    broken: {:3}, delivered: {:3}".format(time.total_seconds(),
                                                                   num_broken, wscountsum))


def create_csv_header():
    columns = ["expnum", "duration", "num_robots", "clever", "num_collisions", "num_broken"]
    for i in range(1, NUM_STATIONS + 1):
        columns.append("wscount" + str(i))
    columns.append("wscountsum")
    return ";".join(columns)


def start(resultdir: str, min_robots: int, max_robots: int, simulations_per_config: int, robots_inc: int,
          max_steps: int, skip_first_simple: bool):
    basepath = Path(resultdir)
    now = datetime.now()
    timestamp = now.strftime("%Y_%m_%d-%H_%M_%S")
    experiment_path = basepath.joinpath("exp02-{}_{}-{}".format(timestamp, min_robots, max_robots))

    num = 2
    while experiment_path.exists():
        experiment_path = basepath.joinpath("exp02-" + timestamp + "_" + "v" + str(num))
    experiment_path.mkdir()


    #logging.config.fileConfig("experiment02.logging.conf")
    logging.basicConfig()
    logger = logging.getLogger(MODULE_LOGGER_NAME)
    logger.setLevel(logging.INFO)
    fh = FileHandler(str(experiment_path / "experiment.log"))
    fh.setLevel(logging.INFO)
    logger.addHandler(fh)

    # experiment = Experiment03(experiment_path, Path("config_3r_20i_5s_200x200.json"))
    # experiment = Experiment03(experiment_path)

    num_exp = 1
    runner = SingleProcessExperimentRunner()

    experiment_file_path = experiment_path.joinpath("experiment.csv")
    with experiment_file_path.open("w") as f:
        f.write(create_csv_header() + "\n")

    for num_robots in range(min_robots, max_robots + 1, robots_inc):
        choices_strategy = [True] if skip_first_simple and num_robots == min_robots else [False, True]
        for clever in choices_strategy:
            for i in range(simulations_per_config):
                if experiment_path.joinpath("stop.txt").exists():
                    return
                else:
                    experiment = Experiment02(num_exp, num_robots, clever, experiment_file_path)
                    experiment.initialize()
                    experiment.step_listeners.append(break_when_all_delivered)
                    experiment.step_listeners.append(break_when_all_broken)
                    logger.info(
                        "Experiment #{:3} --  robots: {:3},  "
                        "clever: {:>5} \t trial {:4}".format(
                            num_exp, num_robots, str(clever), i))
                    experiment.run(max_steps=max_steps)
                    num_exp += 1


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Simple Delivery Robots Experiment 2")

    parser.add_argument('--resultdir', type=str, help='the directory in which experiment results will be stored',
                        default="experiment_results")

    parser.add_argument('min_robots', type=int, help='minimum robots')
    parser.add_argument('max_robots', type=int, help='maximum robots')
    parser.add_argument('--robots_inc', type=int, help='increment for number of robots', default=5)
    parser.add_argument('--simulations_per_config', type=int, help='simulations per config', default=10)
    parser.add_argument('--max_steps', type=int, help='maximum steps', default=500)
    parser.add_argument('--skip-first-simple', const=True, action='store_const', default=False,
                        help='skip first run of simulations with simple strategy')

    args = parser.parse_args()
    print(args)
    MODULE_LOGGER_NAME = 'salma'
    NUM_ITEMS = 100
    NUM_STATIONS = 10
    GRID_WIDTH = 500
    GRID_HEIGHT = 500
    N_SLOTS = 5
    P_SLOT = 0.01
    COLLISION_PROB = 1.0
    # SEED = int(datetime.now().timestamp())
    SEED = 1438470243
    start(args.resultdir, args.min_robots, args.max_robots, args.simulations_per_config, args.robots_inc,
          args.max_steps, args.skip_first_simple)
