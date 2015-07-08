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


class PropertyEvaluationTest03(BaseWorldTest):
    def setUp(self):
        super().setUp()
        world = World.instance()
        world.get_exogenous_action("finish_step").config.occurrence_distribution = ConstantDistribution(
            "integer", 1)

    def test_load_properties_from_string(self):
        world = World.instance()
        e = Experiment(world)
        propspec = """
INVARIANT F : forall(r:robot, forall(i:item,
        until({time_limit1},
            implies(
                occur(grab(r,i)),
                until({time_limit2},
                    carrying(r,i),
                    not(carrying(r,i))
                )
            ),
            xpos(r) >= {xgoal}
        )
))

  goal g: forall(r:robot, xpos(r) >= {xgoal})

"""
        e.property_collection.load_from_string(propspec, time_limit1=100, time_limit2=50, xgoal=50)
        print(e.property_collection.properties)
        self.assertEqual(e.property_collection["f"][0],
                         "forall(r:robot, forall(i:item,until(100,implies(occur(grab(r,i)),"
                         "until(50,carrying(r,i),not(carrying(r,i)))),xpos(r) >= 50)))")
        self.assertEqual(e.property_collection["f"][1], ACHIEVE)

        self.assertEqual(e.property_collection["g"][0], "forall(r:robot, xpos(r) >= 50)")

        self.assertEqual(e.property_collection["g"][1], ACHIEVE)

    def test_load_properties_from_file(self):
        world = World.instance()
        e = Experiment(world)
        e.property_collection.load_from_file("testdata/test1.spsl", time_limit1=100, time_limit2=50, xgoal=50)
        print(e.property_collection.properties)


if __name__ == '__main__':
    unittest.main()
