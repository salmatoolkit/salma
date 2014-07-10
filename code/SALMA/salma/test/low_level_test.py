from salma.engine import EclipseCLPEngine
from salma.model.world import World
from salma.SALMAException import SALMAException
import logging

__author__ = 'Christian'

import unittest


class LowLevelTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):

        try:
            World.set_logic_engine(EclipseCLPEngine("ecl-test/domaindesc.ecl",
                                                    "ecl-test/example_procedures.ecl"))
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('salmalab')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)



if __name__ == '__main__':
    unittest.main()
