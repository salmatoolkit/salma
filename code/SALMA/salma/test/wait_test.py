from salma.experiment import Experiment
from salma.model.world import World
from salma.model.core import Entity
from salma.model.distributions import BernoulliDistribution, ConstantDistribution, OptionalDistribution, \
    ExponentialDistribution

from salma.model.agent import Agent
from salma.model import process
from salma.model.procedure import Procedure, Act, Wait, FunctionStatement
from salma.test.world_test_base import BaseWorldTest
from salma.model.evaluationcontext import EvaluationContext


class WaitTest(BaseWorldTest):
    def __configure_events_default(self):
        world = World.instance()
        finish_step = world.get_exogenous_action("finish_step")
        finish_step.config.occurrence_distribution = ConstantDistribution("integer", 5)
        accidental_drop = world.get_exogenous_action("accidental_drop")
        accidental_drop.config.occurrence_distribution = OptionalDistribution(0.0,
                                                                              ExponentialDistribution("integer", 0.1))
        collision = world.get_exogenous_action("collision")
        collision.config.occurrence_distribution = BernoulliDistribution(1.0)

    def test_wakeup_condition(self):
        self.__configure_events_default()
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 12)
        end_time = -1

        def record_state(ctx=None, **kwargs):
            """
            :param EvaluationContext evaluation_context: ec
            """
            nonlocal end_time
            end_time = ctx.get_current_time()

        pmain = Procedure([
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)"),
            FunctionStatement(record_state)
        ])

        proc = process.OneShotProcess(pmain)
        agent = Agent("rob1", "robot", [proc])
        world.addAgent(agent)

        world.initialize(False)
        world.set_fluent_value("xpos", ["rob1"], 10)
        world.set_fluent_value("ypos", ["rob1"], 15)
        self.initialize_robot("rob1", 10, 15, 0, 0)
        self.setNoOneCarriesAnything()
        print("INIT:")
        print("----")
        world.printState()
        print("----\n\n")

        experiment = Experiment(world)
        experiment.run_until_finished()
        print("----")
        world.printState()
        print("----\n\n")
        print("end_time: {}".format(end_time))
        self.assertEqual(12, end_time)
        self.assertEqual(11, world.get_fluent_value("xpos", ["rob1"]))
        self.assertFalse(world.get_fluent_value("moving", ["rob1"]))

    def test_wakeup_timeout(self):
        self.__configure_events_default()
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 12)

        end_time = -1
        moving_after_wait = None

        def record_state(ctx=None, agent=None):
            """
            :param EvaluationContext ctx: ec
            """
            nonlocal end_time, moving_after_wait
            end_time = ctx.get_current_time()

            moving_after_wait = ctx.get_fluent_value("moving", agent)
            print("Recorded time: {} - moving: {}".format(end_time, moving_after_wait))

        pmain = Procedure([
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)", timeout=5),
            FunctionStatement(record_state)
        ])

        proc = process.OneShotProcess(pmain)
        agent = Agent("rob1", "robot", [proc])
        world.addAgent(agent)

        world.initialize(False)
        world.set_fluent_value("xpos", ["rob1"], 10)
        world.set_fluent_value("ypos", ["rob1"], 15)
        self.initialize_robot("rob1", 10, 15, 0, 0)
        self.setNoOneCarriesAnything()
        print("INIT:")
        print("----")
        world.printState()
        print("----\n\n")

        experiment = Experiment(world)
        experiment.run_until_finished()
        print("----")
        world.printState()
        print("----\n\n")
        print("end_time: {}".format(end_time))
        self.assertEqual(5, end_time)
        self.assertEqual(11, world.get_fluent_value("xpos", ["rob1"]))
        self.assertTrue(moving_after_wait)
        # the event should have happended anyway!
        self.assertFalse(world.get_fluent_value("moving", ["rob1"]))
