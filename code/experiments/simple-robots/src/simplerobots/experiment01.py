from io import TextIOBase
import logging
from salma.experiment import Experiment, SingleProcessExperimentRunner
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution, Never, BernoulliDistribution, NEVER
from salma.model.world import World
from src.simplerobots.agents import create_robot, create_base

MODULE_LOGGER_NAME = 'salma'
logging.basicConfig()
logger = logging.getLogger(MODULE_LOGGER_NAME)
logger.setLevel(logging.DEBUG)


def create_position_logger(fd: TextIOBase):
    def __l(world: World, step=None, **kwargs):
        positions = []
        for rob in world.getDomain("robot"):
            positions.append((rob.id, rob.xpos, rob.ypos))
        columns = [step]
        for p in sorted(positions):
            columns.extend(p[1:])
        fd.write(";".join(list(map(str, columns))) + "\n")
        fd.flush()
    return __l


class Experiment01(Experiment):

    def __init__(self):
        super().__init__("ecl-src/simple-robots-domaindesc.ecl")

    def create_initial_situation(self):
        rob1, item1, item2, base = self.world.get_entities_by_id("rob1", "item1", "item2", "base")
        rob1.xpos = 100
        rob1.ypos = 100
        rob1.vx = 0
        rob1.vy = 0
        rob1.broken = False
        rob1.next_task = None
        rob1.robot_radius = 1
        rob1.set_carrying(item1, False)

        item1.xpos = 200
        item1.ypos = 50
        item1.delivered_to = None

        item2.xpos = 10
        item2.ypos = 80
        item2.delivered_to = None

        base.stationX = 10
        base.stationY = 20

    def create_entities(self):
        rob1 = create_robot(1)
        item1 = Entity("item1", "item")
        item2 = Entity("item2", "item")
        base = create_base()
        self.world.add(rob1, item1, item2, base)

    def setup_distributions(self):
        world = self.world
        world.deactivate_info_transfer()

        world.get_exogenous_action("finish_step").config.occurrence_distribution = \
            ConstantDistribution("integer", 1)
        jump_action = world.get_stochastic_action("jump")
        land_on = jump_action.outcome("land_on")
        land_on.map_param("r", "r")
        land_on.uniform_param("x", (100, 500))
        land_on.uniform_param("y", (0, 200))

        crash = jump_action.outcome("crash")
        crash.map_param("r", "r")

        world.get_exogenous_action(
            "accidental_drop").config.occurrence_distribution = NEVER

        collision_event = world.get_exogenous_action("collision")
        collision_event.config.occurrence_distribution = BernoulliDistribution(0.3)
        collision_event.config.uniform_param("severity", value_range=(0, 10))


if __name__ == '__main__':
    experiment = Experiment01()
    experiment.initialize()
    runner = SingleProcessExperimentRunner()
    with open("simple_robots_positions.csv", "w") as f:
        f.write("step;x1;y1\n")
        f.flush()
        experiment.step_listeners.append(create_position_logger(f))
        _, res, trial_infos = runner.run_trials(experiment, number_of_trials=1, max_steps=500)
    experiment.world.printState()
