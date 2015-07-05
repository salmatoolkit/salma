import unittest
from enum import Enum

from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution
from salma.experiment import Experiment
from salma.model.procedure import Act, Procedure, While, Wait, If
from salma.model.process import OneShotProcess, PeriodicProcess
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest
from salma.constants import *


class TestModes(Enum):
    full = 1
    quick = 2

MODE = TestModes.quick


class PropertyEvaluationTest02(BaseWorldTest):
    def setUp(self):
        super().setUp()
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)

    def perform_property_3_test(self, item, pos_grab, min_pos, goal_pos):
        world = World.instance()

        p = OneShotProcess([
            While("xpos(self) < params[0]", [goal_pos], [
                If("xpos(self) == params[0]", [pos_grab], Act("paint", [SELF, item])),
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

    def test_property_3_ok(self):
        verdict, results = self.perform_property_3_test("item1", 13, 12, 20)
        self.assertEqual(verdict, OK)
        world = World.instance()
        world.printState()
        self.assertTrue(world.getFluentValue("painted", ["item1"]))
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 20)

    def test_property_3_fail(self):
        verdict, results = self.perform_property_3_test("item1", 13, 15, 20)
        self.assertEqual(verdict, NOT_OK)
        world = World.instance()
        world.printState()
        self.assertTrue(world.getFluentValue("painted", ["item1"]))
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 13)

    def create_periodic_agent(self, agent_id, item_id, target_x=40, start=0, period=6, delta=3):
        p1 = OneShotProcess([
            While("xpos(self) < params[0]", [target_x], [
                Act("move_right", [SELF]),
                Wait("not moving(self)")])
        ])
        proc2 = Procedure([
            Act("grab", [SELF, item_id])
        ])
        proc3 = Procedure([
            Act("drop", [SELF, item_id])
        ])
        p2 = PeriodicProcess(proc2, period, introduction_time=start)
        p3 = PeriodicProcess(proc3, period, introduction_time=start + delta)
        rob = Agent(agent_id, "robot", [p1, p2, p3])
        return rob

    def __test_nested_until(self, world: World, outer_time_limit=50, inner_time_limit=4,
                            target_x_f=20, target_x_g=35) -> (int, dict):
        f_str = """
forall(r:robot, forall(i:item,
        until({},
            implies(
                occur(grab(r,i)),
                until({},
                    carrying(r,i),
                    not(carrying(r,i))
                )
            ),
            xpos(r) >= {}
        )
))
""".format(outer_time_limit, inner_time_limit, target_x_f)

        g_str = """
forall(r:robot,
   xpos(r) >=  {}
)
""".format(target_x_g)
        experiment = Experiment(world)
        experiment.property_collection.register_property("f", f_str, INVARIANT)
        experiment.property_collection.register_property("g", g_str, ACHIEVE)
        verdict, results = experiment.run_experiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        # : :type: timedelta
        dt = results["time"]
        print("time: {}".format(dt.total_seconds()))

        # world.printState()
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

    def test_nested_until_fail_one_agent_outer(self):
        world = World.instance()
        rob1 = self.create_periodic_agent("rob1", "item1")
        rob2 = self.create_periodic_agent("rob2", "item2", target_x=18)
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=0)

        verdict, results = self.__test_nested_until(world)
        self.assertEqual(verdict, NOT_OK)

    def test_property_4(self):
        world = World.instance()

        p1 = OneShotProcess([
            While("xpos(self) < 50", [
                Act("move_right", [SELF]),
                Wait("not moving(self)"),
                While("ypos(self) < 15", [
                    Act("move_down", [SELF]),
                    Wait("not moving(self)")
                ]),
                While("ypos(self) > 10", [
                    Act("move_up", [Entity.SELF]),
                    Wait("not moving(self)")
                ])
            ])
        ])

        rob1 = Agent("rob1", "robot", [p1])
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)
        world.setFluentValue("xpos", ["rob1"], 5)

        g_str = """
forall(r:robot,
    until(500,
        implies(
            ypos(r) = 10,
            until(10, true, ypos(r) = 15)
        ),
        xpos(r) >= 50
    )
)
"""

        experiment = Experiment(world)
        experiment.property_collection.register_property("g", g_str, INVARIANT)
        experiment.property_collection.register_property("h", "forall(r:robot, xpos(r) >= 50)", ACHIEVE)
        verdict, results = experiment.run_experiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        # : :type: timedelta
        dt = results["time"]
        print("time: {}".format(dt.total_seconds()))
        world.printState()
        # world.printState()
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
        g_str = """
forall(r:robot,
   xpos(r) >=  50
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
        experiment = Experiment(world)
        experiment.property_collection.register_property("f", f_str, INVARIANT)
        experiment.property_collection.register_property("g", g_str, ACHIEVE)
        verdict, results = experiment.run_experiment()
        print("Verdict: " + str(verdict))
        # print("Results: " + str(results))
        #print(world.logic_engine().format_failure_stack(results["failure_stack"]))
        world.printState()
        self.assertEqual(verdict, OK)

    def _test_variable_match(self, m1=42, m2=100, m3=52, m4=110):
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
        p1_1 = OneShotProcess(Act("mark", [SELF, "item1", m1]), introduction_time=0)
        p2_1 = OneShotProcess(Act("mark", [SELF, "item2", m2]), introduction_time=5)
        p1_2 = OneShotProcess(Act("mark", [SELF, "item1", m3]), introduction_time=15)
        p2_2 = OneShotProcess(Act("mark", [SELF, "item2", m4]), introduction_time=20)

        rob1 = Agent("rob1", "robot", [p1_1, p1_2])
        rob2 = Agent("rob2", "robot", [p2_1, p2_2])
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=10)

        e = Experiment(world)
        e.property_collection.register_property("f", f_str, INVARIANT)
        verdict, results = e.run_experiment(maxSteps=25)
        return verdict, results

    @withHeader()
    def test_variable_match_ok(self):
        verdict, results = self._test_variable_match()
        print("Verdict: " + str(verdict))
        self.assertEqual(verdict, OK)

    @withHeader()
    def test_variable_match_not_ok(self):
        verdict, results = self._test_variable_match(m1=42, m2=100, m3=53, m4=110)
        print("Verdict: " + str(verdict))
        self.assertEqual(verdict, NOT_OK)


if __name__ == '__main__':
    unittest.main()

