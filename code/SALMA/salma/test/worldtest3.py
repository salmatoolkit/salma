import logging
import pprint
import unittest

from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions
from salma.model.agent import Agent
from salma.model.core import  Entity, Fluent, Action, Constant
from salma.model.actions import StochasticAction, DeterministicAction, Stepwise
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    ActionExecution, Procedure, While, VariableAssignment, ArbitraryAction, Variable, \
    Iterate, SelectFirst, ProcedureRegistry, ProcedureCall, If, Plan
from salma.model.world import World
from salma.test.testhelpers import withHeader
import os
from salma.test.world_test_base import BaseWorldTest


def printValue(value):
    print("Val: ", value)
    return (ControlNode.CONTINUE, None)


class WorldTest3(BaseWorldTest):

    @withHeader
    def test_load_declaration_empty(self):
        world = World.instance()
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  ActionExecution("move_right", [Entity.SELF]))
        proc = Procedure("main", [], w)

        agent1 = Agent("rob1", "robot", proc)
        world.addAgent(agent1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        fl, const = world.check_fluent_initialization()
        print(fl)
        print(const)
        self.assertEqual(len(fl), 7)
        self.assertEqual(len(const), 2)
        a, a2 = world.check_action_initialization()
        print("---------")
        for act in a:
            print(str(act[0]) + " : " + str(act[1]))
        for act in a2:
            print(str(act[0]) + " : " + str(act[1]))
        # self.assertEqual(len(a), 1)
        # self.assertEqual(len(a2), 2)
        print("----")
        print(world.describe_actions())
        self.assertEqual(len(list(world.getAllActions())), 12)

    @withHeader
    def test_load_declaration_full(self):
        world = World.instance()
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  ActionExecution("move_right", [Entity.SELF]))
        proc = Procedure("main", [], w)

        agent1 = Agent("rob1", "robot", proc)
        world.addAgent(agent1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        world.get_exogenous_action("accidental_drop").config.set_probability(0.7)
        world.get_exogenous_action("collision").config.set_probability(0.02).normal_param("severity", 0.5, 0.2)
        world.get_stochastic_action("jump").outcome(
            "land_on").map_param("r","r").uniform_param("x",(0,1024)).uniform_param("y",(0,768))

        world.get_stochastic_action("jump").selection_strategy = Stepwise(land_on=0.7, crash=0.2) # 0.2 is wrong here!
        world.get_stochastic_action("jump").outcome("crash").map_param("r","r")
        print(world.describe_actions())
        a, a2 = world.check_action_initialization()

        print(a)
        print(a2)

    def test_property_1(self):
        world = World.instance()

        proc = Procedure("main", [], [
            ActionExecution("paint", [Entity.SELF, "item1"]),
            While(EvaluationContext.PYTHON_EXPRESSION,
                  "xpos(self) < 20", [], [
                    ActionExecution("move_right", [Entity.SELF])
                ])
        ])
        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1","item"))
        world.addEntity(Entity("item2","item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        f_str="""
forall([r,robot],
    implies(
        occur(paint(r,?)),
        until(5,
            true,
            xpos(r) >= 25
        )
    )
)
"""
        world.registerProperty("f", f_str)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        world.printState()

if __name__ == '__main__':
    unittest.main()
