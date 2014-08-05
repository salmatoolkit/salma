import unittest

from salma.model.agent import Agent
from salma.model.core import Entity, Constant
from salma.model.actions import Stepwise
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Act, Procedure, While
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest
from salma.constants import *


def printValue(value):
    print("Val: ", value)
    return (ControlNode.CONTINUE, None)


class WorldTest3(BaseWorldTest):
    @withHeader()
    def test_load_declaration_empty(self):
        world = World.instance()
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  Act("move_right", [Entity.SELF]))
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
        self.assertEqual(len(list(world.getAllActions())), 14)

    @withHeader()
    def test_load_declaration_full(self):
        world = World.instance()
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  Act("move_right", [Entity.SELF]))
        proc = Procedure("main", [], w)

        agent1 = Agent("rob1", "robot", proc)
        world.addAgent(agent1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        world.get_exogenous_action("accidental_drop").config.set_probability(0.7)
        world.get_exogenous_action("collision").config.set_probability(0.02).normal_param("severity", 0.5, 0.2)
        world.get_stochastic_action("jump").outcome(
            "land_on").map_param("r", "r").uniform_param("x", (0, 1024)).uniform_param("y", (0, 768))

        world.get_stochastic_action("jump").selection_strategy = Stepwise(land_on=0.7, crash=0.2)  # 0.2 is wrong here!
        world.get_stochastic_action("jump").outcome("crash").map_param("r", "r")
        print(world.describe_actions())
        a, a2 = world.check_action_initialization()

        print(a)
        print(a2)

    @withHeader(msg="Tests an until property that should fail.")
    def test_property_1_fail(self):
        world = World.instance()

        proc = Procedure("main", [], [
            Act("move_right", [Entity.SELF]),
            Act("paint", [Entity.SELF, "item1"]),
            While(EvaluationContext.PYTHON_EXPRESSION,
                  "xpos(self) < 20", [], [
                    Act("move_right", [Entity.SELF])
                ])
        ])
        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        f_str = """
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
        world.registerProperty("f", f_str, World.INVARIANT)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        world.printState()
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertEqual(verdict, NOT_OK)

    @withHeader(msg="Tests an until property that should succeed.")
    def test_property_1_OK(self):
        world = World.instance()

        proc = Procedure("main", [], [
            Act("move_right", [Entity.SELF]),
            Act("paint", [Entity.SELF, "item1"]),
            While(EvaluationContext.PYTHON_EXPRESSION,
                  "xpos(self) < 30", [], [
                    Act("move_right", [Entity.SELF])
                ])
        ])
        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=23)

        f_str = """
forall([r,robot],
    implies(
        occur(paint(r,?)),
        until(5,
            true,
            xpos(r) > 25
        )
    )
)
"""
        world.registerProperty("f", f_str, World.INVARIANT)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        world.printState()
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertEqual(verdict, OK)

    @withHeader(msg="Tests an until property that is undetermined.")
    def test_property_1_nondet(self):
        world = World.instance()

        proc = Procedure("main", [], [
            Act("move_right", [Entity.SELF]),
            Act("paint", [Entity.SELF, "item1"]),
            While(EvaluationContext.PYTHON_EXPRESSION,
                  "xpos(self) < 30", [], [
                    Act("move_right", [Entity.SELF])
                ])
        ])
        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        f_str = """
forall([r,robot],
    implies(
        occur(paint(r,?)),
        until(50,
            true,
            xpos(r) > 50
        )
    )
)
"""
        world.registerProperty("f", f_str, World.INVARIANT)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        world.printState()
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertEqual(verdict, NONDET)

    def test_property_2(self):
        world = World.instance()

        proc = Procedure("main", [], [
            Act("paint", [Entity.SELF, "item1"]),
            While(EvaluationContext.PYTHON_EXPRESSION,
                  "xpos(self) < 20", [], [
                    Act("move_right", [Entity.SELF])
                ])
        ])
        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)
        world.setFluentValue("xpos", ["rob1"], 5)

        g_str = """
forall([r,robot], until(200, xpos(r) > 6, xpos(r) > 10))
"""
        world.registerProperty("g", g_str, World.INVARIANT)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        # world.printState()
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertEqual(verdict, NOT_OK)

    def test_property_3(self):
        world = World.instance()

        proc = Procedure("main", [], [
            # Act("paint", [Entity.SELF, "item1"]),
            While(EvaluationContext.PYTHON_EXPRESSION,
                  "xpos(self) < 20", [], [
                    Act("move_right", [Entity.SELF])
                ])
        ])
        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)
        world.setFluentValue("xpos", ["rob1"], 5)

        g_str = """
        implies(occur(paint(rob1,?)), xpos(rob1) > 200)
"""
        world.registerProperty("g", g_str, World.INVARIANT)
        world.registerProperty("h", "xpos(rob1) >= 20", World.ACHIEVE)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        # world.printState()
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertEqual(verdict, OK)

    def test_property_4(self):
        world = World.instance()

        proc = Procedure("main", [], [
            # Act("paint", [Entity.SELF, "item1"]),
            While(EvaluationContext.PYTHON_EXPRESSION,
                  "xpos(self) < 20", [], [
                    Act("move_right", [Entity.SELF]),
                    While(EvaluationContext.PYTHON_EXPRESSION,
                          "ypos(self) < 15", [], Act("move_down", [Entity.SELF])),
                    While(EvaluationContext.PYTHON_EXPRESSION,
                          "ypos(self) > 10", [], Act("move_up", [Entity.SELF]))
                ])
        ])

        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)
        world.setFluentValue("xpos", ["rob1"], 5)

        g_str = """
forall([r,robot],
    until(200,
        implies(
            ypos(r) = 10,
            until(10, true, ypos(r) = 15)
        ),
        xpos(r) >= 20
    )
)
"""
        world.registerProperty("g", g_str, World.INVARIANT)
        world.registerProperty("h", "xpos(rob1) >= 20", World.ACHIEVE)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        # world.printState()
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        # self.assertEqual(verdict, OK)

        if __name__ == '__main__':
            unittest.main()
