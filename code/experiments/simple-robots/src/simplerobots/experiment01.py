from io import TextIOBase
import logging
from logging import FileHandler
import logging.config
from math import sqrt
from numpy import random
from salma.constants import OK, NOT_OK
from salma.experiment import Experiment, SingleProcessExperimentRunner
from salma.model.core import Entity, translate_entities
from salma.model.distributions import ConstantDistribution, Never, BernoulliDistribution, NEVER, GeometricDistribution, \
    CustomDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.selectionstrategy import NonDeterministic, Categorical
from salma.model.world import World
from simplerobots.agents import create_robot, create_coordinator, create_coordinator_clever
import numpy as np
import json
from pathlib import Path
from datetime import datetime
import argparse


def create_step_logger(fd: TextIOBase):
    def __l(world: World, step=None, **kwargs):
        positions = []
        assignments = []
        brokenstates = []
        robots = sorted(world.getDomain("robot"))
        for rob in robots:
            positions.append((rob.xpos, rob.ypos))
            assignments.append(rob.next_task)
            brokenstates.append(rob.broken)
        columns = [step, world.time]
        for p in positions:
            columns.extend(p)
        columns.extend(assignments)
        columns.extend(brokenstates)
        for ws in sorted(world.getDomain("workstation")):
            columns.append(ws.delivered_item_count)
        coordinator = world.getEntityById("coordinator1")
        columns.append(translate_entities(coordinator.request_queue))
        fd.write(";".join(list(map(str, columns))) + "\n")
        fd.flush()

    return __l


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


class Experiment01(Experiment):
    def __init__(self, logpath: Path, initpath: Path = None):
        super().__init__("ecl-src/simple-robots-domaindesc.ecl")
        self.logpath = logpath
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
        if STRATEGY == "simple":
            coordinator1 = create_coordinator()
        elif STRATEGY == "clever":
            coordinator1 = create_coordinator_clever()
        else:
            raise RuntimeError("Unsupported strategy: %s" % STRATEGY)

        self.world.add(coordinator1)
        for r in range(1, NUM_ROBOTS + 1):
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

        collision_event.config.uniform_param("severity", value_range=(5, 10))

        request_event = world.get_exogenous_action("request")

        def request_distrib(ws, c, ctx: EvaluationContext = None, **kwargs):
            ws_in_queue = c.request_queue.count(ws)
            assigned_robots = len([r for r in ctx.getDomain("robot") if r.task_workstation == ws])
            n_free = N_SLOTS - assigned_robots - ws_in_queue
            p_tot = 1 - (1 - P_SLOT) ** n_free
            return None if p_tot == 0 else np.random.geometric(p_tot)

        request_event.config.occurrence_distribution = CustomDistribution("integer", request_distrib)

    def __create_report(self):
        report = dict()
        report["seed"] = SEED
        robots = dict()
        for r in self.world.getDomain("robot"):
            robots[r.id] = {
                "x": r.xpos,
                "y": r.ypos
            }
        report["robots"] = robots
        items = dict()
        for i in self.world.getDomain("item"):
            items[i.id] = {
                "x": i.xpos,
                "y": i.ypos,
                "delivered_to": translate_entities(i.delivered_to)
            }
        report["items"] = items
        workstations = dict()
        for ws in self.world.getDomain("workstation"):
            workstations[ws.id] = {
                "x": ws.stationX,
                "y": ws.stationY,
                "delivered_item_count": ws.delivered_item_count
            }
        report["workstations"] = workstations
        coordinators = dict()
        for c in self.world.getDomain("coordinator"):
            coordinators[c.id] = {
                "request_queue": translate_entities(c.request_queue)
            }
        report["coordinators"] = coordinators
        return report

    def before_run(self, **kwargs):
        report = self.__create_report()
        with self.logpath.joinpath("before.json").open("w") as fd:
            json.dump(report, fd)

    def after_run(self, verdict, **kwargs):
        report = self.__create_report()
        with self.logpath.joinpath("after.json").open("w") as fd:
            json.dump(report, fd)


def create_csv_header():
    columns = ["step", "time"]
    for i in range(1, NUM_ROBOTS + 1):
        columns.append("x" + str(i))
        columns.append("y" + str(i))
    for i in range(1, NUM_ROBOTS + 1):
        columns.append("task" + str(i))
    for i in range(1, NUM_ROBOTS + 1):
        columns.append("broken" + str(i))
    for i in range(1, NUM_STATIONS + 1):
        columns.append("wscount" + str(i))
    columns.append("queue")
    return ";".join(columns)


def start(resultdir: str, configfile: str):
    basepath = Path(resultdir)
    print(("storing results in {}".format(basepath)))

    now = datetime.now()
    timestamp = now.strftime("%Y_%m_%d-%H_%M_%S")
    experiment_path = basepath.joinpath(timestamp + "_" + STRATEGY)
    num = 2
    while experiment_path.exists():
        experiment_path = basepath.joinpath(timestamp + "_" + "v" + str(num))
    experiment_path.mkdir()

    MODULE_LOGGER_NAME = 'salma'
    # logging.config.fileConfig("experiment01.logging.conf")
    logging.basicConfig()
    logger = logging.getLogger(MODULE_LOGGER_NAME)
    logger.setLevel(logging.DEBUG)
    fh = FileHandler(str(experiment_path / "experiment.log"))
    fh.setLevel(logging.DEBUG)
    logger.addHandler(fh)

    experiment = Experiment01(experiment_path, Path(configfile))
    experiment.initialize()
    runner = SingleProcessExperimentRunner()

    with experiment_path.joinpath("experiment.csv").open("w") as f:
        f.write(create_csv_header() + "\n")
        f.flush()
        experiment.step_listeners.append(create_step_logger(f))
        experiment.step_listeners.append(break_when_all_delivered)
        experiment.step_listeners.append(break_when_all_broken)
        # _, res, trial_infos = runner.run_trials(experiment, number_of_trials=1, max_steps=3000, max_retrials=0)
        experiment.run(max_steps=5000)
    experiment.world.printState()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Simple Delivery Robots Experiment 1')
    parser.add_argument('configfile', type=str, help='the JSON configuration file')

    parser.add_argument('--resultdir', type=str, help='the directory in which experiment results will be stored',
                        default="experiment_results")

    parser.add_argument('--strategy', type=str, help="the coordinator's strategy,", choices=["simple", "clever"],
                        default="simple")

    args = parser.parse_args()
    print(args)
    NUM_ROBOTS = 3
    NUM_ITEMS = 20
    NUM_STATIONS = 5
    GRID_WIDTH = 200
    GRID_HEIGHT = 200
    N_SLOTS = 5
    P_SLOT = 0.01
    COLLISION_PROB = 0
    # SEED = int(datetime.now().timestamp())
    SEED = 1438470243
    STRATEGY = args.strategy
    start(args.resultdir, args.configfile)
