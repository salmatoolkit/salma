
__author__ = 'kroiss'

import unittest
import networkx as nx
import os

class GraphMLTest(unittest.TestCase):
    def test_loadgraphml(self):
        print(os.getcwd())
        #: :type : Graph
        g = nx.read_graphml("../../../testdata/test1.graphml")
        print(g.nodes(data=True))
        print(g.edges(data=True))


if __name__ == '__main__':
    unittest.main()
