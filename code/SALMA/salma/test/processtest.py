import logging
import unittest
from salma.model.world import World
from salma.engine import EclipseCLPEngine
from salma import SMCException
from salma.model.core import Entity
from salma.model.distributions import Distribution, BernoulliDistribution

from salma.test.testhelpers import withHeader
from salma.model.agent import Agent
from salma.model import process
from salma.model.procedure import Procedure, ActionExecution, While, Sequence
from salma.test.world_test_base import BaseWorldTest
from salma import constants
from salma.model.evaluationcontext import EvaluationContext

class ProcessTest(BaseWorldTest):

    @withHeader
    def test_one_shot_process(self):
        world = World.instance()
        seq = Sequence()
        seq.addChild(ActionExecution("move_right", [Entity.SELF]))
        seq.addChild(ActionExecution("move_down", [Entity.SELF]))

        proc = process.OneShotProcess(Procedure("main", [], seq))
        agent = Agent("rob1", "robot", [proc])
        world.addAgent(agent)

        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)

        self.setNoOneCarriesAnything()
        print("INIT:")
        print("----")
        world.printState()
        print("----\n\n")

        verdict, info = world.runUntilFinished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))
        print("----")
        world.printState()
        print("----\n\n")
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 16)
        self.assertEqual(verdict, constants.OK)
        self.assertEqual(info['steps'], 4)

        self.assertEqual(proc.execution_count, 1)
        self.assertEqual(proc.introduction_time, 0)
        self.assertEqual(proc.last_start_time, 0)
        self.assertEqual(proc.last_end_time, 2)

    @withHeader
    def test_two_one_shot_processes(self):
        world = World.instance()
        seq = Sequence()
        seq.addChild(ActionExecution("move_right", [Entity.SELF]))
        seq.addChild(ActionExecution("move_down", [Entity.SELF]))

        proc1 = process.OneShotProcess(Procedure("main", [], seq))

        seq2 = Sequence()
        seq2.addChild(ActionExecution("move_left", [Entity.SELF]))
        seq2.addChild(ActionExecution("move_up", [Entity.SELF]))

        proc2 = process.OneShotProcess(Procedure("main", [], seq2))

        agent = Agent("rob1", "robot", [proc1, proc2])
        world.addAgent(agent)

        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)

        self.setNoOneCarriesAnything()
        print("INIT:")
        print("----")
        world.printState()
        print("----\n\n")

        verdict, info = world.runUntilFinished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))
        print("----")
        world.printState()
        print("----\n\n")

        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 10)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 15)
        self.assertEqual(verdict, constants.OK)
        self.assertEqual(info['steps'], 4)
        self.assertEqual(proc1.execution_count, 1)
        self.assertEqual(proc1.introduction_time, 0)
        self.assertEqual(proc1.last_start_time, 0)
        self.assertEqual(proc1.last_end_time, 2)
        self.assertEqual(proc2.execution_count, 1)
        self.assertEqual(proc2.introduction_time, 0)
        self.assertEqual(proc2.last_start_time, 0)
        self.assertEqual(proc2.last_end_time, 2)

    @staticmethod
    def create_agent_with_periodic_processes():
        """
        :rtype: Agent, list of process.PeriodicProcess
        """
        seq = Sequence()
        seq.addChild(ActionExecution("move_right", [Entity.SELF]))
        control_procedure = Procedure("main", [], seq)
        proc1 = process.PeriodicProcess(control_procedure, 10)

        seq2 = Sequence()
        seq2.addChild(ActionExecution("move_down", [Entity.SELF]))
        control_procedure2 = Procedure("main", [], seq2)
        proc2 = process.PeriodicProcess(control_procedure2, 5)

        agent = Agent("rob1", "robot", [proc1, proc2])
        return agent, [proc1, proc2]

    @withHeader
    def test_periodic_process_lower(self):
        world = World.instance()

        agent, processes = ProcessTest.create_agent_with_periodic_processes()
        world.addAgent(agent)
        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)

        self.setNoOneCarriesAnything()

        verdict, info = world.runUntilFinished(maxWorldTime=90)
        self.assertEqual(19, world.getFluentValue("xpos",["rob1"]))
        self.assertEqual(33, world.getFluentValue("ypos",["rob1"]))
        world.printState()

    @withHeader
    def test_periodic_process_higher(self):
        world = World.instance()
        agent, processes = ProcessTest.create_agent_with_periodic_processes()
        world.addAgent(agent)

        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)
        self.setNoOneCarriesAnything()
        verdict, info = world.runUntilFinished(maxWorldTime=91)
        self.assertEqual(20, world.getFluentValue("xpos",["rob1"]))
        self.assertEqual(34, world.getFluentValue("ypos",["rob1"]))
        world.printState()

    @withHeader
    def test_triggered_process(self):
        world = World.instance()

        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom",
                  [Entity.SELF, 50],
                  ActionExecution("move_right", [Entity.SELF]))
        proc1 = process.OneShotProcess(Procedure("main", [], w))
        handler_seq = Sequence([
            ActionExecution("move_down", [Entity.SELF])])
        handler = process.TriggeredProcess(Procedure("handler",[],handler_seq),
                                           EvaluationContext.PYTHON_EXPRESSION,
                                           "xpos(self) == 25", [])
        agent = Agent("rob1", "robot", [proc1, handler])
        world.addAgent(agent)
        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 10)
        self.setNoOneCarriesAnything()
        verdict, info = world.runUntilFinished(maxWorldTime=50)
        world.printState()
        self.assertEqual(50, world.getFluentValue("xpos",["rob1"]))
        self.assertEqual(11, world.getFluentValue("ypos",["rob1"]))
