import unittest

from salma.model.agent import Agent
from salma.model.core import Entity, Constant
from salma.model.distributions import ConstantDistribution
from salma.model.experiment import Experiment
from salma.model.selectionstrategy import Stepwise
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Act, Procedure, While, Wait, If
from salma.model.process import OneShotProcess, PeriodicProcess
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest
from salma.constants import *
from datetime import datetime, timedelta


class PropertyEvaluationTest02(BaseWorldTest):

    def setUp(self):
        super().setUp()
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)

    def perform_property_3_test(self, item, pos_grab, min_pos, goal_pos):
        world = World.instance()

        p = OneShotProcess([
            While("xpos(self) < 20", [
                If("xpos(self) == 13", [pos_grab], Act("paint", [SELF, item])),
                Act("move_right", [SELF]),
                Wait("not moving(self)")
            ])
        ])
        rob1 = Agent("rob1", "robot", [p])
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        g_str = """
        implies(occur(paint(rob1,?)), xpos(rob1) > {})
""".format(min_pos)

        def logger(world, **info):
            print("X = {}".format(world.getFluentValue("xpos", ["rob1"])))

        experiment = Experiment(world)
        experiment.step_listeners.append(logger)
        experiment.property_collection.register_property("g", g_str, INVARIANT)
        experiment.property_collection.register_property("h", "xpos(rob1) >= {}".format(goal_pos), ACHIEVE)
        verdict, results = experiment.run_experiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        return verdict, results

    def test_property_e_ok(self):
        verdict, results = self.perform_property_3_test("item1", 13, 12, 20)
        self.assertEqual(verdict, OK)
        world = World.instance()
        world.printState()
        self.assertTrue(world.getFluentValue("painted", ["item1"]))

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
forall(r:robot, forall(i:item,
        until(50,
            implies(
                occur(grab(r,i)),
                until(4,
                    carrying(r,i),
                    not(carrying(r,i))
                )
            ),
            xpos(r) >= 20
        )
))
"""

        g_str = """
forall([r, robot],
   xpos(r) >=  35
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
        rob1 = self.create_periodic_agent("rob1", "item1", start=0)
        rob2 = self.create_periodic_agent("rob2", "item2", start=0)
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
        rob2 = self.create_periodic_agent("rob2", "item2", period=10, delta=7)
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        verdict, results = self.__test_nested_until(world)
        self.assertEqual(verdict, NOT_OK)

    # @unittest.skip
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
        rob2 = self.create_periodic_agent("rob2", "item2", target_x=50)
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
        # print("Results: " + str(results))
        print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        self.assertEqual(verdict, OK)

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

