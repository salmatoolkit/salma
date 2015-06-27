from salma.constants import SELF, INVARIANT, ACHIEVE
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution, OptionalDistribution, ExponentialDistribution, \
    BernoulliDistribution, NormalDistribution, Categorical, ComposedDistribution, GeometricDistribution
from salma.model.experiment import Experiment
from salma.model.procedure import Act, While, Wait
from salma.model.process import OneShotProcess


class SMCTestBaseExperiment(Experiment):
    def __init__(self, num_robots, p_drop, drop_delay_mean, drop_delay_std, p_collision,
                 time_limit, x_goal, x_goal2):
        super().__init__("ecl-test/domaindesc.ecl")
        self.num_robots = num_robots
        self.p_drop = p_drop
        self.drop_delay_mean = drop_delay_mean
        self.drop_delay_std = drop_delay_std
        self.p_collision = p_collision
        self.time_limit = time_limit
        self.x_goal = x_goal
        self.x_goal2 = x_goal2

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
            Act("grab", [SELF, "item" + str(num)]),
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
        items = self.world.getDomain('item')
        for i in items:
            self.world.setFluentValue('carrying', [robot_id, i.id], False)

    def setup_distributions(self):
        world = self.world
        world.deactivate_info_transfer()

        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)

        probs = [(None, 0.8), (0.05, 0.02), (0.06, 0.02), (0.07, 0.02), (0.08, 0.02), (0.09, 0.02),
                 (0.1, 0.02), (0.11, 0.02), (0.12, 0.02), (0.13, 0.02), (0.14, 0.02)]

        pd = Categorical("float", probs)

        # world.get_exogenous_action(
        #     "accidental_drop").config.occurrence_distribution = OptionalDistribution(
        #     self.p_drop,
        #     NormalDistribution("integer", self.drop_delay_mean, self.drop_delay_std))
        world.get_exogenous_action(
            "accidental_drop").config.occurrence_distribution = ComposedDistribution(GeometricDistribution,
                                                                                     "integer", pd)

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
            occur(grab(r, i)),
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
