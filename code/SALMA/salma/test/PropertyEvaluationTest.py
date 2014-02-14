from salma.test.world_test_base import BaseWorldTest
from salma.model.world import World
__author__ = 'kroiss'

class PropertyEvaluationTest(BaseWorldTest):

    def test_simple_property_OK(self):
        world = World.instance()
        world.runExperiment()

