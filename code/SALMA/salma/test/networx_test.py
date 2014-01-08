from sphinx.ext.graphviz import graphviz

__author__ = 'kroiss'

import unittest
import networkx as nx
import matplotlib.pyplot as plt

def select_removal_candidate(graph):
    """
    :type graph: networkx.classes.graph.Graph
    :rtype: tuple
    """
    for u, v in graph.edges_iter():
        if nx.local_edge_connectivity(graph, u, v) > 1:
            return u, v
    return None


class NetworkXTest(unittest.TestCase):
    def test_create_graph(self):
        g = nx.Graph()
        g.add_nodes_from([1,2,3])
        g.add_edge(1,3, weight=0.5)
        g.add_edge(2,3, weight=0.5)
        g.add_edge(3,1, weight=0.5)
        nx.draw(g)

        plt.show()

    def test_geom_graph(self):
        g = nx.random_geometric_graph(30, 0.3)
        nx.draw(g)
        plt.show()
        candidate = select_removal_candidate(g)
        while candidate is not None:
            g.remove_edge(*candidate)
            candidate = select_removal_candidate(g)

        nx.draw(g)
        plt.show()
        print(g.nodes(data=True))

if __name__ == '__main__':
    unittest.main()
