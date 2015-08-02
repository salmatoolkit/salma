import numpy as np

from salma.SALMAException import SALMAException
from salma.constants import SELF, INVARIANT, ACHIEVE
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution, BernoulliDistribution, CategoricalDistribution, \
    CustomDistribution
from salma.experiment import Experiment
from salma.model.procedure import Act, While, Wait, If, Variable
from salma.model.process import OneShotProcess


def generate_drop_delay_distribution(drop_probabilities):
    """
    :type drop_probabilities: list[float]
    """

    def accidental_drop_delay(r, i, grip=None, **ctx):
        g = grip(r)
        if g < 1 or g > len(drop_probabilities):
            raise SALMAException("No drop rate defined for "
                                 "grip quality {}.".format(g))
        p = drop_probabilities[g - 1]
        if p == 0:
            return None
        else:
            return np.random.geometric(p)

    return accidental_drop_delay


class SMCTestBaseExperiment2(Experiment):
    def __init__(self, config):
        """
        Creates an experiment instance with the given configuration map that is loaded from the JSON config file.
        :type config: dict[str, obj]
        """
        super().__init__("ecl-test/smctest02_base_domain.ecl")
        self.num_robots = config["num_robots"]
        self.p_collision = config["p_collision"]
        self.time_limit = config["time_limit"]
        self.grip_probs = [(i + 1, gp) for i, gp in enumerate(config["grip_probs"])]
        self.drop_probs = config["drop_probs"]
        self.destination_range = config["destination_range"]

    def __place_agents_in_column(self, x):
        y = 10
        for r in self.world.getDomain('robot'):
            self.initialize_robot(r.id, x, y, 0, 0)
            y += 20
        for i in self.world.getDomain("item"):
            self.world.set_fluent_value("xpos", [i.id], x)
            self.world.set_fluent_value("ypos", [i.id], y)
            y += 20

    def create_robot(self, num):
        """
        Creates a simple agent that grabs an item with id item+num and keeps moving right as long as the agent is active
        """
        myItem = Variable("myItem")
        proc = OneShotProcess([
            Act("pickUp", [SELF, myItem]),
            While("xpos(self) < destX(myItem)", [
                Act("move_right", [SELF]),
                Wait("not moving(self)")
            ]),
            If("carrying(self, myItem)",
               Act("drop", [SELF, myItem]))
        ])
        agent = Agent("rob" + str(num), "robot", [proc], myItem="item" + str(num))
        return agent

    def initialize_robot(self, robot_id, x, y, vx, vy):
        self.world.set_fluent_value("xpos", [robot_id], x)
        self.world.set_fluent_value("ypos", [robot_id], y)
        self.world.set_fluent_value("vx", [robot_id], vx)
        self.world.set_fluent_value("vy", [robot_id], vy)
        self.world.set_constant_value("robot_radius", [robot_id], 1)
        self.world.set_fluent_value("active", [robot_id], True)
        self.world.set_fluent_value("partner", [robot_id], None)
        self.world.set_fluent_value("grip", [robot_id], 0)
        items = self.world.getDomain('item')
        for i in items:
            self.world.set_fluent_value('carrying', [robot_id, i.id], False)

    def setup_distributions(self):
        world = self.world
        world.deactivate_info_transfer()

        world.get_exogenous_action("finish_step").config.occurrence_distribution = \
            ConstantDistribution("integer", 1)
        pickup = world.get_stochastic_action("pickUp")
        grab = pickup.outcome("grab")
        grab.map_param("r", "r"), grab.map_param("i", "i")
        grab.set_param_distribution("grip", CategoricalDistribution("integer", self.grip_probs))

        drop_delay_fn = generate_drop_delay_distribution(self.drop_probs)

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
        for i in self.world.getDomain("item"):
            dist = np.random.randint(self.destination_range[0], self.destination_range[1] + 1)
            self.world.set_constant_value("destX", [i.id], self.world.get_fluent_value("xpos", [i.id]) + dist)
            self.world.set_constant_value("destY", [i.id], self.world.get_fluent_value("ypos", [i.id]))

        self.world.set_constant_value("gravity", [], 9.81)

    def setup_properties(self):
        f_str = """
forall(r:robot,
    forall(i:item,
        implies(
            occur(grab(r, i, ?)),
            until({time_limit},
                carrying(r, i),
                xpos(i) = destX(i)
            )
        )
    )
)
"""
        self.property_collection.register_property("f", f_str, INVARIANT,
                                                   time_limit=self.time_limit)

        g_str = """
forall(i:item, and(
    xpos(i) = destX(i),
    not(exists(r:robot, carrying(r, i) ))))
"""
        self.property_collection.register_property("g", g_str, ACHIEVE)
