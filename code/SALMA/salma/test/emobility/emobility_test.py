__author__ = 'kroiss'

import unittest
from salma.model.world import World
from salma.engine import EclipseCLPEngine
from salma import SMCException
import logging
import salma
import os

from salma.model.distributions import BernoulliDistribution


class EMobilityTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):

        try:
            World.set_logic_engine(EclipseCLPEngine("../../../ecl-test/e-mobility/e-mobility-domain.ecl"))
        except SMCException as e:
            print(e)
            raise
        logger = logging.getLogger('salma')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)

    def setUp(self):
        World.create_new_world()
        world = World.instance()
        world.load_declarations()

    def testWorldCreation(self):
        world = World.instance()
        world.printState()


if __name__ == '__main__':
    unittest.main()
