import logging
import unittest
import itertools
import math
from math import sqrt
from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions, process
from salma.model.core import Entity, Fluent, Action, Constant
from salma.model.actions import DeterministicAction, StochasticAction, \
    RandomActionOutcome, Uniform
from salma.model.selectionstrategy import OutcomeSelectionStrategy, Uniform
from salma.model.events import ExogenousAction
from salma.model.agent import Agent
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution, ConstantDistribution, OptionalDistribution, \
    ExponentialDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    Act, Procedure, While, Assign, ArbitraryAction, Variable, \
    Iterate, Select, ProcedureRegistry, ProcedureCall, If, Plan
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest


def printValue(value):
    print("Val: ", value)
    return ControlNode.CONTINUE, None


class MySelectionStrategy(OutcomeSelectionStrategy):
    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluation_context, param_values):
        """
        :type evaluation_context: EvaluationContext
        :type param_values: list
        """
        x = evaluation_context.getFluentValue('xpos', param_values[0])
        height = param_values[1]
        if x > 100 or height > 50:
            return self.options["crash"]
        else:
            return self.options["land_on"]


class WorldTest(BaseWorldTest):
    def __configure_events_default(self):
        world = World.instance()
        finish_step = world.get_exogenous_action("finish_step")
        finish_step.config.occurrence_distribution = ConstantDistribution("integer", 5)
        accidental_drop = world.get_exogenous_action("accidental_drop")
        accidental_drop.config.occurrence_distribution = OptionalDistribution(0.0,
                                                                              ExponentialDistribution("integer", 0.1))
        collision = world.get_exogenous_action("collision")
        collision.config.occurrence_distribution = BernoulliDistribution(1.0)

    @withHeader()
    def testWorldStepExplicit(self):
        world = World.instance()
        world.addAgent(self.create_right_moving_mobot('rob1'))
        world.initialize(False)
        self.__configure_events_default()
        self.initialize_robot("rob1", 10, 10, 0, 0)

        print("\n\n----\n\n")
        print("INIT:")
        print("\n\n----\n\n")
        world.printState()

        verdict, finished, toplevel_results, *details = world.step(100)

        print("\n\n----\n\n")
        print("AFTER STEP 1:")
        print("\n\n----\n\n")
        world.printState()
        self.assertEqual(world.getTime(), 5)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 10)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 10)
        self.assertEqual(world.getFluentValue("vx", ["rob1"]), 1)
        self.assertEqual(world.getFluentValue("vy", ["rob1"]), 0)

        world.step(100)

        print("\n\n----\n\n")
        print("AFTER STEP 2:")
        print("\n\n----\n\n")
        world.printState()

        self.assertEqual(world.getTime(), 10)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 10)
        self.assertEqual(world.getFluentValue("vx", ["rob1"]), 0)
        self.assertEqual(world.getFluentValue("vy", ["rob1"]), 1)
        world.step(100)
        print("\n\n----\n\n")
        print("AFTER STEP 3:")
        print("\n\n----\n\n")
        world.printState()
        self.assertEqual(world.getTime(), 100)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("vx", ["rob1"]), 0)
        self.assertEqual(world.getFluentValue("vy", ["rob1"]), 0)