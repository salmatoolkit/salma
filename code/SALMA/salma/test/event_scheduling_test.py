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
    BernoulliDistribution, NormalDistribution
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

        world.setConstantValue("world_width", [], 500)
        world.setConstantValue("world_height", [], 500)
        world.setConstantValue("safety_distance", [], 10)

        rob1 = self.create_random_walk_robot("rob1")
        world.addAgent(rob1)
        world.initialize(False)
        world.setFluentValue("xpos", [rob1.id], 250)
        world.setFluentValue("ypos", [rob1.id], 250)
        world.setFluentValue("vx", [rob1.id], 1)
        world.setFluentValue("vy", [rob1.id], 0)
        world.setFluentValue("active", [rob1.id], True)
        world.setFluentValue("wheels_wet", [rob1.id], False)
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
        verdict, info = e1.run_experiment(max_world_time=500, step_listeners=[steplogger])
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
            e1.run_experiment(max_world_time=500, step_listeners=[steplogger])
            self.fail("Expected exception!")
        except SALMAException as ex:
            print("As expected: {}".format(ex.message))



if __name__ == '__main__':
    unittest.main()


