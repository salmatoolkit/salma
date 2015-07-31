import logging
import random
import unittest

from salma.SALMAException import SALMAException
from salma.constants import INVARIANT, ACHIEVE, OK
from salma.engine import EclipseCLPEngine
from salma.model import process
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import ExponentialDistribution, ConstantDistribution, \
    BernoulliDistribution, NormalDistribution, CustomDistribution, GeometricDistribution
from salma.experiment import Experiment
from salma.model.procedure import Sequence, Assign, Act, Variable
from salma.model.selectionstrategy import Categorical
from salma.model.world import World


def steplogger(world: World, **kwargs):
    print("Step: {} - T = {} - pos: ({}, {}) - v: ({}, {})".format(
        kwargs["step"],
        world.getTime(),
        world.get_fluent_value("xpos", ["rob1"]),
        world.get_fluent_value("ypos", ["rob1"]),
        world.get_fluent_value("vx", ["rob1"]),
        world.get_fluent_value("vy", ["rob1"])))
    return True, None


class EventSchedulingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            World.set_logic_engine(
                EclipseCLPEngine("ecl-test/event_scheduling/domaindesc_event_scheduling.ecl"))
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('salmalab')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)
        logger2 = logging.getLogger('salma.model')
        logger2.setLevel(logging.DEBUG)
        logger2.addHandler(ch)

    def setUp(self):
        print("setup")
        World.create_new_world()
        world = World.instance()
        world.add_additional_expression_context_global("random", random)
        world.load_declarations()
        world.deactivate_all_events()

    def create_random_walk_robot(self, robotId):
        def change_direction(agent: Agent=None, xpos=None, ypos=None, vx=None, vy=None, **kwargs):
            """
            :type xpos: (str) -> int
            :type ypos: (str) -> int
            :type vx: (str) -> int
            :type vy: (str) -> int
            """
            if xpos(agent.id) > 490:
                nvx = -1
            elif xpos(agent.id) < 10:
                nvx = 1
            else:
                nvx = vx(agent.id)

            if ypos(agent.id) > 490:
                nvy = -1
            elif ypos(agent.id) < 10:
                nvy = 1
            else:
                nvy = vy(agent.id)
            return nvx, nvy

        seq = Sequence([
            Assign(("nvx", "nvy"), change_direction),
            Act("set_velocity", [Entity.SELF, Variable("nvx"), Variable("nvy")])
        ])

        proc = process.TriggeredProcess(seq, "occur('wall_alert', self)", [])
        agent = Agent(robotId, "robot", [proc])
        return agent

    def setup_world(self):
        world = World.instance()

        world.set_constant_value("world_width", [], 500)
        world.set_constant_value("world_height", [], 500)
        world.set_constant_value("safety_distance", [], 10)

        rob1 = self.create_random_walk_robot("rob1")
        world.addAgent(rob1)
        world.initialize(False)
        world.set_fluent_value("xpos", [rob1.id], 250)
        world.set_fluent_value("ypos", [rob1.id], 250)
        world.set_fluent_value("vx", [rob1.id], 1)
        world.set_fluent_value("vy", [rob1.id], 0)
        world.set_fluent_value("active", [rob1.id], True)
        world.set_fluent_value("wheels_wet", [rob1.id], False)
        world.deactivate_info_transfer()

    def test_movement(self):
        self.setup_world()
        world = World.instance()
        lightning = world.get_exogenous_action("lightning_strike")
        lightning.config.occurrence_distribution = ExponentialDistribution("integer", 0.1)

        asteroid_hit = world.get_exogenous_action("asteroid_hit")
        asteroid_hit.config.occurrence_distribution = ConstantDistribution("integer", 10)
        asteroid_hit.config.set_param_distribution("size", NormalDistribution("float", 100.0, 25.0))

        disaster = world.get_exogenous_action_choice("disaster")
        disaster.selection_strategy = Categorical(lightning_strike=0.1, asteroid_hit=0.9)

        wall_alert = world.get_exogenous_action("wall_alert")
        wall_alert.config.occurrence_distribution = BernoulliDistribution(1.0)

        e1 = Experiment(world)
        # e1.run_until_finished(max_world_time=500, step_listeners=[steplogger])
        e1.property_collection.register_property("f", "let(mx : 500-5, xpos(rob1) =\= mx)", INVARIANT)
        e1.property_collection.register_property("g", "xpos(rob1) =:= 240", ACHIEVE)
        # e1.property_collection.register_property("h", "not(occur(asteroid_hit(rob1, ?)))", INVARIANT)
        verdict, info = e1.run(max_world_time=500, step_listeners=[steplogger])
        self.assertEqual(verdict, OK)
        print("T = {}, verdict = {}\n\ninfo: {}".format(world.getTime(), verdict, info))
        # TODO: actually check (statistically) that choice of events for disaster works correctly

    def test_flawed_stepwise_distrib(self):
        self.setup_world()
        world = World.instance()
        lightning = world.get_exogenous_action("lightning_strike")
        lightning.config.occurrence_distribution = ExponentialDistribution("integer", 0.1)

        asteroid_hit = world.get_exogenous_action("asteroid_hit")
        asteroid_hit.config.occurrence_distribution = ConstantDistribution("integer", 10)
        asteroid_hit.config.set_param_distribution("size", NormalDistribution("float", 100.0, 25.0))

        disaster = world.get_exogenous_action_choice("disaster")
        # use wrong probabilities that don't sum up to 1.0
        disaster.selection_strategy = Categorical(lightning_strike=0.1, asteroid_hit=0.7)

        wall_alert = world.get_exogenous_action("wall_alert")
        wall_alert.config.occurrence_distribution = BernoulliDistribution(1.0)

        e1 = Experiment(world)
        try:
            e1.run(max_world_time=500, step_listeners=[steplogger])
            self.fail("Expected exception!")
        except SALMAException as ex:
            print("As expected: {}".format(ex.message))

    def init_robot(self, robot, x=250, y=250, vx=1, vy=0):
        robot.xpos = x
        robot.ypos = y
        robot.vx = vx
        robot.vy = vy
        robot.active = True
        robot.wheels_wet = False

    def test_entity_arguments_passed_as_entities(self):
        world = World.instance()

        rob1 = self.create_random_walk_robot("rob1")
        rob2 = self.create_random_walk_robot("rob2")

        world.add(rob1, rob2)

        world.initialize(False)
        world.deactivate_info_transfer()

        world.world_width = 500
        world.world_height = 500
        world.safety_distance = 10
        self.init_robot(rob1, 250, 250, 1, 0)
        self.init_robot(rob2, 255, 250, 0, 0)
        reclist1 = []
        reclist2 = []

        def _collision_occurrence_distrib(r1, r2, **ctx):
            reclist1.append(r1)
            reclist1.append(r2)
            return True

        def _collision_severity_distrib(r1, r2, **ctx):
            reclist2.append(r1)
            reclist2.append(r2)
            return 42

        collision = world.get_exogenous_action("collide")
        collision.config.occurrence_distribution = CustomDistribution("boolean", _collision_occurrence_distrib)
        collision.config.set_param_distribution("severity",
                                                CustomDistribution("integer", _collision_severity_distrib))

        e1 = Experiment(world)
        e1.run(max_world_time=0, step_listeners=[steplogger])
        print("rec1:", reclist1)
        print("rec2:", reclist2)
        self.assertListEqual(reclist1, [rob1, rob2])
        self.assertListEqual(reclist2, [rob1, rob2])

    def test_custom_distrib_lambda(self):
        world = World.instance()

        rob1 = self.create_random_walk_robot("rob1")
        rob2 = self.create_random_walk_robot("rob2")

        world.add(rob1, rob2)

        world.initialize(False)
        world.deactivate_info_transfer()

        world.world_width = 500
        world.world_height = 500
        world.safety_distance = 10
        self.init_robot(rob1, 250, 250, 1, 2)
        self.init_robot(rob2, 255, 250, 3, 4)

        collision = world.get_exogenous_action("collide")
        collision.config.occurrence_distribution = BernoulliDistribution(1.0)
        sev = lambda r1, r2: 1000 * r1.vx + 100 * r1.vy + 10 * r2.vx + r2.vy

        collision.config.set_param_distribution("severity", CustomDistribution("integer", sev))
        e1 = Experiment(world)
        reclist = []

        def recorder(w, **kwargs):
            reclist.append(kwargs)

        e1.step_listeners.append(recorder)
        e1.run(max_world_time=0)
        print(reclist)

    def test_event_instance_scheduled_only_once(self):
        world = World.instance()

        rob1 = self.create_random_walk_robot("rob1")
        rob2 = self.create_random_walk_robot("rob2")

        world.add(rob1, rob2)

        world.initialize(False)
        world.deactivate_info_transfer()

        world.world_width = 500
        world.world_height = 500
        world.safety_distance = 10
        self.init_robot(rob1, 250, 250, 0, 0)
        self.init_robot(rob2, 300, 250, 0, 0)
        welcome = world.get_exogenous_action("welcome_message")
        welcome.config.occurrence_distribution = GeometricDistribution(0.1)
        welcome.config.uniform_param("code", (0, 10))

        e1 = Experiment(world)
        reclist = []

        def recorder(w, **kwargs):
            reclist.append(kwargs)

        e1.step_listeners.append(recorder)
        e1.run(max_world_time=100)
        print(reclist)


if __name__ == '__main__':
    unittest.main()
