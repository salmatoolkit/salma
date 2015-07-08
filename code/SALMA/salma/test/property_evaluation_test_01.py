import unittest

from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import ConstantDistribution
from salma.experiment import Experiment
from salma.model.procedure import Act, Procedure, While, Wait
from salma.model.process import OneShotProcess
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest
from salma.constants import *
from salma.psl.internaldsl import *


class PropertyEvaluationTest01(BaseWorldTest):
    def setUp(self):
        super().setUp()
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)

    def perform_property_1_test(self, x, x_loop_goal, x_prop_goal, time_limit):
        world = World.instance()

        proc = Procedure([
            Act("move_right", [SELF]),
            Wait("not moving(self)"),
            Act("paint", [SELF, "item1"]),
            While("xpos(self) < params[0]", [x_loop_goal], [
                Act("move_right", [SELF]),
                Wait("not moving(self)")
            ])
        ])
        rob1 = Agent("rob1", "robot", proc)
        world.addAgent(rob1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.place_agents_in_column(x=x)

        f = Forall("r", "robot",
                   Implies(
                       Occur("paint", ["r", "?"]),
                       Until("{time_limit}", True, "xpos(r) >= {x_prop_goal}")
                   ))
        print("Testing property: {}".format(str(f)))
        experiment = Experiment(world)
        experiment.step_listeners.append(lambda world, **args: print(world.getFluentValue("xpos", ["rob1"])))
        experiment.property_collection.register_property("f", f, INVARIANT, time_limit=time_limit,
                                                         x_prop_goal=x_prop_goal)
        verdict, results = experiment.run_experiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        world.printState()
        return verdict, results

    @withHeader(msg="Tests an until property that should fail.")
    def test_property_1_fail(self):
        world = World.instance()
        verdict, results = self.perform_property_1_test(10, 20, 25, 5)
        self.assertEqual(verdict, NOT_OK)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 16)

    @withHeader(msg="Tests an until property that should succeed.")
    def test_property_1_OK(self):
        world = World.instance()
        verdict, results = self.perform_property_1_test(23, 30, 25, 5)
        self.assertEqual(verdict, OK)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 30)

    @withHeader(msg="Tests an until property that is undetermined.")
    def test_property_1_nondet(self):
        world = World.instance()
        verdict, results = self.perform_property_1_test(10, 30, 50, 50)
        self.assertEqual(verdict, NONDET)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 30)

    def perform_property_2_test(self, agentstop1=20, agentstop2=20, agentstart1=10, agentstart2=10, timelimit=50, px=5,
                                qx=18) -> (int, dict):
        world = World.instance()

        p1 = OneShotProcess([
            Act("paint", [SELF, "item1"]),
            While("xpos(self) < params[0]", [agentstop1], [
                Act("move_right", [SELF]),
                Wait("not moving(self)")])
        ])
        p2 = OneShotProcess([
            Act("paint", [SELF, "item1"]),
            While("xpos(self) < params[0]", [agentstop2], [
                Act("move_right", [SELF]),
                Wait("not moving(self)")])
        ])
        rob1 = Agent("rob1", "robot", [p1])
        rob2 = Agent("rob2", "robot", [p2])
        world.addAgent(rob1)
        world.addAgent(rob2)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.initialize(False)
        self.setNoOneCarriesAnything()
        self.initialize_robot("rob1", agentstart1, 10, 0, 0)
        self.initialize_robot("rob2", agentstart2, 20, 0, 0)

        experiment = Experiment(world)
        g_str = """
forall([r,robot], until({}, xpos(r) > {}, xpos(r) > {}))
""".format(timelimit, px, qx)
        experiment.property_collection.register_property("g", g_str, INVARIANT)
        verdict, results = experiment.run_experiment()
        print("Verdict: " + str(verdict))
        print("Results: " + str(results))
        return verdict, results

    def test_property_2_ok(self):
        world = World.instance()
        verdict, results = self.perform_property_2_test()
        self.assertEqual(verdict, OK)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 20)
        self.assertEqual(world.getFluentValue("xpos", ["rob2"]), 20)

    def test_property_2_nondet_rob1(self):
        verdict, results = self.perform_property_2_test(agentstop1=15)
        self.assertEqual(verdict, NONDET)

    def test_property_2_nondet_rob2(self):
        verdict, results = self.perform_property_2_test(agentstop2=15)
        self.assertEqual(verdict, NONDET)

    def test_property_2_not_ok_rob1_fails_p(self):
        verdict, results = self.perform_property_2_test(agentstart1=4)
        self.assertEqual(verdict, NOT_OK)

    def test_property_2_not_ok_rob2_fails_p(self):
        verdict, results = self.perform_property_2_test(agentstart2=4)
        self.assertEqual(verdict, NOT_OK)

    def test_property_2_not_ok_rob1_fails_q(self):
        verdict, results = self.perform_property_2_test(agentstart1=6, timelimit=6, qx=15)
        self.assertEqual(verdict, NOT_OK)

    def test_property_2_not_ok_rob2_fails_q(self):
        verdict, results = self.perform_property_2_test(agentstart2=6, timelimit=6, qx=15)
        self.assertEqual(verdict, NOT_OK)


if __name__ == '__main__':
    unittest.main()
