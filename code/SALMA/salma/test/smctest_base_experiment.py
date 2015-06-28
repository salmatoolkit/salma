from salma.SALMAException import SALMAException
from salma.constants import SELF, INVARIANT, ACHIEVE
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution, OptionalDistribution, ExponentialDistribution, \
    BernoulliDistribution, NormalDistribution, Categorical, ComposedDistribution, GeometricDistribution, Distribution, \
    CustomDistribution
from salma.model.experiment import Experiment
from salma.model.procedure import Act, While, Wait
from salma.model.process import OneShotProcess
import numpy as np


def generate_drop_delay_distribution(drop_probabilities):
    """
    :type drop_probabilities: list[float]
    """

    def accidental_drop_delay(r, i, grip=None, **ctx):
        g = grip(r)
        if g < 1 or g > len(drop_probabilities):
            raise SALMAException("No drop rate defined for "
                                 "grip quality {}.".format(g))
        p = drop_probabilities[g-1]
        if p == 0:
            return None
        else:
            return np.random.geometric(p)

    return accidental_drop_delay


class SMCTestBaseExperiment(Experiment):
    def __init__(self, config):
        """
        Creates an experiment instance with the given configuration map that is loaded from the JSON config file.
        :type config: dict[str, obj]
        """
        super().__init__("ecl-test/smctest_base_domain.ecl")
        self.num_robots = config["num_robots"]
        self.p_collision = config["p_collision"]
        self.time_limit = config["time_limit"]
        self.x_goal = config["x_goal"]
        self.x_goal2 = config["x_goal2"]
        self.grip_probs = [(i+1, gp) for i, gp in enumerate(config["grip_probs"])]
        self.drop_props = config["drop_probs"]

    def __place_agents_in_column(self, x):
        y = 10
        for r in self.world.getDomain('robot'):
            self.initialize_robot(r.id, x, y, 0, 0)
            y += 20

    def create_robot(self, num):
        """
        Creates a simple agent that grabs an item with id item+num and keeps moving right as long as the agent is active
        """
        proc = OneShotProcess([
            Act("pickUp", [SELF, "item" + str(num)]),
            While("True", [
                Act("move_right", [SELF]),
                Wait("not moving(self)")
            ])
        ])
        agent = Agent("rob" + str(num), "robot", [proc])
        return agent

    def initialize_robot(self, robot_id, x, y, vx, vy):
        self.world.setFluentValue("xpos", [robot_id], x)
        self.world.setFluentValue("ypos", [robot_id], y)
        self.world.setFluentValue("vx", [robot_id], vx)
        self.world.setFluentValue("vy", [robot_id], vy)
        self.world.setConstantValue("robot_radius", [robot_id], 1)
        self.world.setFluentValue("active", [robot_id], True)
        self.world.setFluentValue("partner", [robot_id], None)
        self.world.setFluentValue("grip", [robot_id], 0)
        items = self.world.getDomain('item')
        for i in items:
            self.world.setFluentValue('carrying', [robot_id, i.id], False)

    def setup_distributions(self):
        world = self.world
        world.deactivate_info_transfer()

        world.get_exogenous_action("finish_step").config.occurrence_distribution = \
            ConstantDistribution("integer", 1)
        pickup = world.get_stochastic_action("pickUp")
        grab = pickup.outcome("grab")
        grab.map_param("r", "r"), grab.map_param("i", "i")
        grab.set_param_distribution("grip", Categorical("integer", self.grip_probs))

        drop_delay_fn = generate_drop_delay_distribution(self.drop_props)

        world.get_exogenous_action(
            "accidental_drop").config.occurrence_distribution = CustomDistribution("integer", drop_delay_fn)

        collision_event = world.get_exogenous_action("collision")
        collision_event.config.occurrence_distribution = BernoulliDistribution(self.p_collision)
        collision_event.config.uniform_param("severity", value_range=(0, 100))

    def create_entities(self):
        for i in range(self.num_robots):
            self.world.addEntity(Entity("item" + str(i + 1), "item"))

        for i in range(self.num_robots):
            self.world.addAgent(self.create_robot(i + 1))

    def augment_world_context(self):
        self.world.register_clp_function("robotLeftFrom")

    def create_initial_situation(self):
        self.__place_agents_in_column(0)
        self.world.setConstantValue("gravity", [], 9.81)

    def setup_properties(self):
        f_str = """
forall(r:robot,
    forall(i:item,
        implies(
            occur(grab(r, i, ?)),
            until({time_limit},
                carrying(r, i),
                xpos(r) > {x_goal}
            )
        )
    )
)
"""
        self.property_collection.register_property("f", f_str, INVARIANT,
                                                   time_limit=self.time_limit, x_goal=self.x_goal)

        g_str = "forall(r:robot, xpos(r) >= {x_goal2})"
        self.property_collection.register_property("g", g_str, ACHIEVE, x_goal2=self.x_goal2)
