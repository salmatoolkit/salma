from cvxopt.misc import scale
from salma.model.core import Entity
from salma.test.emobility.map_generator import MapGenerator
from salma.test.emobility.map_translator import MapTranslator

__author__ = 'kroiss'

import unittest
from salma.model.world import World
from salma.engine import EclipseCLPEngine
from salma import SMCException
import logging
import salma
import os
import random
from salma.model.distributions import BernoulliDistribution
import matplotlib.pyplot as plt
import networkx as nx

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
        self.logger = logging.getLogger('salma')
        World.create_new_world()
        world = World.instance()
        world.load_declarations()

    def testWorldCreation(self):
        world = World.instance()



        mgen = MapGenerator()
        m = mgen.generate_map(10,5,15,1000,1000)
        mt = MapTranslator()
        mt.init_world_from_graph(m, world)
        print(world.getSorts())

        locs = world.getDomain("location")
        for l in locs:
            x = world.getConstantValue("locX", [l.id])
            y = world.getConstantValue("locY", [l.id])
            print("{}: x={}, y={}".format(l,x,y))

        world.printState()



        nx.draw_spring(m, scale=2.5)

        plt.show()




if __name__ == '__main__':
    unittest.main()
