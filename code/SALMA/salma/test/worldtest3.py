import unittest

from salma.model.agent import Agent
from salma.model.core import Entity, Constant
from salma.model.actions import Stepwise
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Act, Procedure, While
from salma.model.process import OneShotProcess, PeriodicProcess
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest
from salma.constants import *
from datetime import datetime, timedelta


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
        self.assertEqual(len(list(world.getAllActions())), 15)

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

    def create_periodic_agent(self, agent_id, item_id, target_x=40, start=0, period=6, delta=3):
        proc = Procedure("main", [], [
            While(EvaluationContext.PYTHON_EXPRESSION,
                  u"xpos(self) < {0:d}".format(target_x), [], [
                    Act("move_right", [Entity.SELF])
                ])
        ])
        p1 = OneShotProcess(proc)
        proc2 = Procedure("main", [], [
            Act("grab", [Entity.SELF, item_id])
        ])
        proc3 = Procedure("main", [], [
            Act("drop", [Entity.SELF, item_id])
        ])
        p2 = PeriodicProcess(proc2, period, introduction_time=start)
        p3 = PeriodicProcess(proc3, period, introduction_time=start + delta)
        rob = Agent(agent_id, "robot", [p1, p2, p3])
        return rob

    def __test_nested_until(self, world: World) -> (int, dict):
        f_str = """
forall([r, robot], forall([i, item],
    until(20,
        implies(
            occur(grab(r, i)),
            until(5,
                carrying(r,i),
                not(carrying(r,i))
            )
        ),
        xpos(r) >= 29
    )
))
"""
        g_str = """
forall([r, robot],
   xpos(r) = 35
)
"""
        world.registerProperty("f", f_str, World.INVARIANT)
        world.registerProperty("g", g_str, World.ACHIEVE)
        verdict, results = world.runExperiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        # : :type: timedelta
        dt = results["time"]
        print("time: {}".format(dt.total_seconds()))

        # world.printState()
        print("\n\n" + ("-" * 50) + "\n")
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        return verdict, results

    @withHeader()
    def test_nested_until_ok(self):
        world = World.instance()
        rob1 = self.create_periodic_agent("rob1", "item1")
        rob2 = self.create_periodic_agent("rob2", "item2")
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        verdict, results = self.__test_nested_until(world)
        self.assertEqual(verdict, OK)

    @withHeader()
    def test_nested_until_fail_one_agent_inner(self):
        world = World.instance()
        rob1 = self.create_periodic_agent("rob1", "item1")
        rob2 = self.create_periodic_agent("rob2", "item2", period=10, delta=6)
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        verdict, results = self.__test_nested_until(world)
        self.assertEqual(verdict, NOT_OK)

    @unittest.skip
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
        # : :type: timedelta
        dt = results["time"]
        print("time: {}".format(dt.total_seconds()))

        # world.printState()
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertEqual(verdict, OK)

    @withHeader()
    def test_property_with_variable_1(self):
        f_str = """
forall(r : robot,
            forall(i : item,
                let(z : xpos(r) + ypos(r) + 5,
                    implies(
                        z = 25,
                        until(20,
                            implies(
                                occur(grab(r, i)),
                                let(startx : xpos(r),
                                    until(10,
                                        xpos(r) >= startx,
                                        not(carrying(r, i))
                                    )
                                )
                            ),
                            let(w : xpos(r) - 1,
                                w > z)
                        )
                    )
                )
            )
        )
"""
        world = World.instance()
        rob1 = self.create_periodic_agent("rob1", "item1", target_x=50)
        rob2 = self.create_periodic_agent("rob2", "item2",  target_x=50)
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)
        world.registerProperty("f", f_str, World.INVARIANT)
        verdict, results = world.runExperiment(maxSteps=50)
        print("Verdict: " + str(verdict))
        #print("Results: " + str(results))
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertAlmostEqual(verdict, OK)

    @withHeader()
    def test_variable_match_ok(self):
        f_str = """
forall(r : robot,
    implies(
        occur(mark(r, ?, ?)),
        match([i, m] : occur(mark(r, i, m)),
            let(m2 : m + 10,
                until(20,
                    true,
                    occur(mark(r, i, m2))
                )
            )
        )
    )
)
"""
        world = World.instance()
        p1_1 = OneShotProcess(Act("mark", [Entity.SELF, "item1", 42]), introduction_time=0)
        p2_1 = OneShotProcess(Act("mark", [Entity.SELF, "item2", 100]), introduction_time=5)
        p1_2 = OneShotProcess(Act("mark", [Entity.SELF, "item1", 52]), introduction_time=15)
        p2_2 = OneShotProcess(Act("mark", [Entity.SELF, "item2", 110]), introduction_time=20)

        rob1 = Agent("rob1", "robot", [p1_1, p1_2])
        rob2 = Agent("rob2", "robot", [p2_1, p2_2])
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)
        world.registerProperty("f", f_str, World.INVARIANT)
        verdict, results = world.runExperiment(maxSteps=25)
        print("Verdict: " + str(verdict))
        self.assertEqual(verdict, OK)

    @withHeader()
    def test_variable_match_not_ok(self):
        f_str = """
forall(r : robot,
    implies(
        occur(mark(r, ?, ?)),
        match([i, m] : occur(mark(r, i, m)),
            let(m2 : m + 10,
                until(20,
                    true,
                    occur(mark(r, i, m2))
                )
            )
        )
    )
)
"""
        world = World.instance()
        p1_1 = OneShotProcess(Act("mark", [Entity.SELF, "item1", 42]), introduction_time=0)
        p2_1 = OneShotProcess(Act("mark", [Entity.SELF, "item2", 100]), introduction_time=5)
        p1_2 = OneShotProcess(Act("mark", [Entity.SELF, "item1", 53]), introduction_time=15)
        p2_2 = OneShotProcess(Act("mark", [Entity.SELF, "item2", 110]), introduction_time=20)

        rob1 = Agent("rob1", "robot", [p1_1, p1_2])
        rob2 = Agent("rob2", "robot", [p2_1, p2_2])
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)
        world.registerProperty("f", f_str, World.INVARIANT)
        verdict, results = world.runExperiment(maxSteps=25)
        print("Verdict: " + str(verdict))
        self.assertEqual(verdict, NOT_OK)

if __name__ == '__main__':
    unittest.main()

