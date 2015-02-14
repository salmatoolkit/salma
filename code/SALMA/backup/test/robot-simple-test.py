import logging
import math
import random
from salma.engine import EclipseCLPEngine
from salma.model.world import World
from salma import  SALMAException

__author__ = 'kroiss'

import unittest


class RobotSimpleTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):

        try:
            World.set_logic_engine(EclipseCLPEngine("ecl-test/robot-simple/robot.simple-domaindesc.ecl"))
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('salmalab')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)

    def setUp(self):
        World.create_new_world()
        world = World.instance()
        world.load_declarations()



if __name__ == '__main__':
    unittest.main()
