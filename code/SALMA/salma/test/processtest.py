from salma.experiment import Experiment
from salma.model.world import World
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution

from salma.test.testhelpers import withHeader
from salma.model.agent import Agent
from salma.model import process
from salma.model.procedure import Procedure, Act, While, Wait
from salma.test.world_test_base import BaseWorldTest
from salma import constants


class ProcessTest(BaseWorldTest):

    def test_one_shot_process(self):
        world = World.instance()
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
        world.set_fluent_value("xpos", ["rob1"], 10)
        world.set_fluent_value("ypos", ["rob1"], 15)
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
        self.assertEqual(world.get_fluent_value("xpos", ["rob1"]), 11)
        self.assertEqual(world.get_fluent_value("ypos", ["rob1"]), 16)
        self.assertEqual(verdict, constants.OK)

        self.assertEqual(proc.execution_count, 1)
        self.assertEqual(proc.introduction_time, 0)
        self.assertEqual(proc.last_start_time, 0)
        # we expect time to be 10 since we use a constant movement delay of 5
        self.assertEqual(proc.last_end_time, 10)

    def test_two_one_shot_processes(self):
        world = World.instance()

        proc1 = process.OneShotProcess([
            Wait("not moving(self)"),
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)"),
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)")])

        proc2 = process.OneShotProcess([
            Wait("not moving(self)"),
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

        self.assertEqual(world.get_fluent_value("xpos", ["rob1"]), 12)
        self.assertEqual(world.get_fluent_value("ypos", ["rob1"]), 17)
        self.assertEqual(verdict, constants.OK)
        self.assertEqual(proc1.execution_count, 1)
        self.assertEqual(proc1.introduction_time, 0)
        self.assertEqual(proc1.last_start_time, 0)
        self.assertTrue(10 <= proc1.last_end_time <= 20)
        self.assertEqual(proc2.execution_count, 1)
        self.assertEqual(proc2.introduction_time, 0)
        self.assertEqual(proc2.last_start_time, 0)
        self.assertTrue(10 <= proc2.last_end_time <= 20)

    @staticmethod
    def create_agent_with_periodic_processes():
        """
        :rtype: Agent, list of process.PeriodicProcess
        """
        control_procedure = Procedure([
            Wait("not moving(self)"),
            Act("move_right", [Entity.SELF]),
            Wait("not moving(self)")])
        proc1 = process.PeriodicProcess(control_procedure, 10)

        control_procedure2 = Procedure([
            Wait("not moving(self)"),
            Act("move_down", [Entity.SELF]),
            Wait("not moving(self)")])
        proc2 = process.PeriodicProcess(control_procedure2, 5)

        agent = Agent("rob1", "robot", [proc1, proc2])
        return agent, [proc1, proc2]

    @withHeader()
    def test_periodic_process_lower(self):
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)
        agent, processes = ProcessTest.create_agent_with_periodic_processes()
        world.addAgent(agent)
        world.initialize(False)
        self.initialize_robot("rob1", 10, 15, 0, 0)
        self.setNoOneCarriesAnything()

        experiment = Experiment(world)
        experiment.run_until_finished(max_world_time=90)
        self.assertEqual(19, world.get_fluent_value("xpos", ["rob1"]))
        self.assertEqual(33, world.get_fluent_value("ypos", ["rob1"]))
        world.printState()

    @withHeader()
    def test_periodic_process_higher(self):
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)
        agent, processes = ProcessTest.create_agent_with_periodic_processes()
        world.addAgent(agent)
        world.initialize(False)
        self.initialize_robot("rob1", 10, 15, 0, 0)
        self.setNoOneCarriesAnything()

        experiment = Experiment(world)
        experiment.run_until_finished(max_world_time=92)
        self.assertEqual(20, world.get_fluent_value("xpos", ["rob1"]))
        self.assertEqual(34, world.get_fluent_value("ypos", ["rob1"]))
        world.printState()

    @withHeader()
    def test_triggered_process(self):
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)

        proc1 = process.OneShotProcess(
            While("robotLeftFrom", [Entity.SELF, 50, "s0"], [
                Wait("not moving(self)"),
                Act("move_right", [Entity.SELF]),
                Wait("not moving(self)")]))

        handler_procedure = Procedure([
            Wait("not moving(self)"),
            Act("move_down", [Entity.SELF]),
            Wait("not moving(self)")])
        proc2 = process.TriggeredProcess(handler_procedure, "self.xpos >= 25 and self.ypos == 10")
        agent = Agent("rob1", "robot", [proc1, proc2])
        world.addAgent(agent)
        world.initialize(False)
        self.initialize_robot("rob1", 10, 10, 0, 0)
        self.setNoOneCarriesAnything()

        experiment = Experiment(world)
        experiment.run_until_finished(max_world_time=50)

        world.printState()
        self.assertEqual(50, world.get_fluent_value("xpos", ["rob1"]))
        self.assertEqual(11, world.get_fluent_value("ypos", ["rob1"]))
