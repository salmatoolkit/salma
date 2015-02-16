from salma.model.experiment import Experiment
from salma.model.world import World
from salma.model.core import Entity
from salma.model.distributions import BernoulliDistribution, ConstantDistribution, OptionalDistribution, \
    ExponentialDistribution

from salma.test.testhelpers import withHeader
from salma.model.agent import Agent
from salma.model import process
from salma.model.procedure import Procedure, Act, While, Sequence, Wait
from salma.test.world_test_base import BaseWorldTest
from salma import constants
from salma.model.evaluationcontext import EvaluationContext


class ProcessTest(BaseWorldTest):
    def __configure_events_default(self):
        world = World.instance()
        finish_step = world.get_exogenous_action("finish_step")
        finish_step.config.occurrence_distribution = ConstantDistribution("integer", 5)
        accidental_drop = world.get_exogenous_action("accidental_drop")
        accidental_drop.config.occurrence_distribution = OptionalDistribution(0.0,
                                                                              ExponentialDistribution("integer", 0.1))
        collision = world.get_exogenous_action("collision")
        collision.config.occurrence_distribution = BernoulliDistribution(1.0)

    def test_one_shot_process(self):
        self.__configure_events_default()
        world = World.instance()
        seq = Sequence()
        pmain = Procedure([
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)"),
            Act("move_down", [Entity.SELF]),
            Wait("not moving(self)")
        ])

        proc = process.OneShotProcess(pmain)
        agent = Agent("rob1", "robot", [proc])
        world.addAgent(agent)

        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)
        self.initialize_robot("rob1", 10, 15, 0, 0)
        self.setNoOneCarriesAnything()
        print("INIT:")
        print("----")
        world.printState()
        print("----\n\n")

        experiment = Experiment(world)
        verdict, info = experiment.run_until_finished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))
        print("----")
        world.printState()
        print("----\n\n")
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 16)
        self.assertEqual(verdict, constants.OK)

        self.assertEqual(proc.execution_count, 1)
        self.assertEqual(proc.introduction_time, 0)
        self.assertEqual(proc.last_start_time, 0)
        # we expect time to be 10 since we use a constant movement delay of 5
        self.assertEqual(proc.last_end_time, 10)

    def test_two_one_shot_processes(self):
        self.__configure_events_default()
        world = World.instance()

        proc1 = process.OneShotProcess([
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)"),
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)")])

        proc2 = process.OneShotProcess([
            Act("move_down", [Entity.SELF]),
            Wait("not moving(self)"),
            Act("move_down", [Entity.SELF]),
            Wait("not moving(self)")])

        agent = Agent("rob1", "robot", [proc2, proc1])
        world.addAgent(agent)

        world.initialize(False)
        self.initialize_robot("rob1", 10, 15, 0, 0)
        self.setNoOneCarriesAnything()

        print("INIT:")
        print("----")
        world.printState()
        print("----\n\n")

        experiment = Experiment(world)
        verdict, info = experiment.run_until_finished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))
        print("----")
        world.printState()
        print("----\n\n")

        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 12)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 17)
        self.assertEqual(verdict, constants.OK)
        self.assertEqual(proc1.execution_count, 1)
        self.assertEqual(proc1.introduction_time, 0)
        self.assertEqual(proc1.last_start_time, 0)
        self.assertEqual(proc1.last_end_time, 10)
        self.assertEqual(proc2.execution_count, 1)
        self.assertEqual(proc2.introduction_time, 0)
        self.assertEqual(proc2.last_start_time, 0)
        self.assertEqual(proc2.last_end_time, 10)

    @staticmethod
    def create_agent_with_periodic_processes():
        """
        :rtype: Agent, list of process.PeriodicProcess
        """
        seq = Sequence()
        seq.add_child(Act("move_right", [Entity.SELF]))
        control_procedure = Procedure("main", [], seq)
        proc1 = process.PeriodicProcess(control_procedure, 10)

        seq2 = Sequence()
        seq2.add_child(Act("move_down", [Entity.SELF]))
        control_procedure2 = Procedure("main", [], seq2)
        proc2 = process.PeriodicProcess(control_procedure2, 5)

        agent = Agent("rob1", "robot", [proc1, proc2])
        return agent, [proc1, proc2]

    @withHeader()
    def test_periodic_process_lower(self):
        world = World.instance()

        agent, processes = ProcessTest.create_agent_with_periodic_processes()
        world.addAgent(agent)
        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)

        self.setNoOneCarriesAnything()

        verdict, info = world.runUntilFinished(maxWorldTime=90)
        self.assertEqual(19, world.getFluentValue("xpos", ["rob1"]))
        self.assertEqual(33, world.getFluentValue("ypos", ["rob1"]))
        world.printState()

    @withHeader()
    def test_periodic_process_higher(self):
        world = World.instance()
        agent, processes = ProcessTest.create_agent_with_periodic_processes()
        world.addAgent(agent)

        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)
        self.setNoOneCarriesAnything()
        verdict, info = world.runUntilFinished(maxWorldTime=91)
        self.assertEqual(20, world.getFluentValue("xpos", ["rob1"]))
        self.assertEqual(34, world.getFluentValue("ypos", ["rob1"]))
        world.printState()

    @withHeader()
    def test_triggered_process(self):
        world = World.instance()

        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom",
                  [Entity.SELF, 50],
                  Act("move_right", [Entity.SELF]))
        proc1 = process.OneShotProcess(Procedure("main", [], w))
        handler_seq = Sequence([
            Act("move_down", [Entity.SELF])])
        handler = process.TriggeredProcess(Procedure("handler", [], handler_seq),
                                           "xpos(self) == 25", [])
        agent = Agent("rob1", "robot", [proc1, handler])
        world.addAgent(agent)
        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 10)
        self.setNoOneCarriesAnything()
        verdict, info = world.runUntilFinished(maxWorldTime=50)
        world.printState()
        self.assertEqual(50, world.getFluentValue("xpos", ["rob1"]))
        self.assertEqual(11, world.getFluentValue("ypos", ["rob1"]))
