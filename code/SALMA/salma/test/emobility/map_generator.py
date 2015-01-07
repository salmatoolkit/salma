import networkx as nx
from networkx.algorithms.connectivity import local_edge_connectivity
import random
import logging
import math
from salma.SALMAException import SALMAException
from salma.model.world import World
import re


class MapGenerator(object):
    logger = logging.getLogger('salmalab')
    P_NODE_IS_STREET_END = 0.4

    def __init__(self, world=None):
        """
        :type world: World
        """
        self.__world = world

    @staticmethod
    def get_dist(graph, node1, node2):
        """
        :type graph: networkx.classes.graph.Graph
        :type node1: str
        :type node2: str
        """
        pos1 = graph.node[node1]["scaled_pos"]
        pos2 = graph.node[node2]["scaled_pos"]
        return math.sqrt((pos2[0]-pos1[0])**2 + (pos2[1]-pos1[1])**2)

    @staticmethod
    def set_roadlength(graph, node1, node2):
        """
        :type graph: networkx.classes.graph.Graph
        :type node1: str
        :type node2: str
        """
        direct_dist = MapGenerator.get_dist(graph, node1, node2)
        # dist = math.ceil(random.uniform(direct_dist, 1.5*direct_dist))
        dist = math.ceil(direct_dist)
        graph.edge[node1][node2]["weight"] = dist

    @staticmethod
    def __select_removal_candidate(graph):
        """
        :type graph: networkx.classes.graph.Graph
        :rtype: tuple
        """
        for u, v in graph.edges_iter():
            if local_edge_connectivity(graph, u, v) > 1:
                return u, v

        return None

    @staticmethod
    def __clean_graph(graph):
        """
        :type graph: networkx.classes.graph.Graph
        """
        candidate = MapGenerator.__select_removal_candidate(graph)
        while candidate is not None:
            graph.remove_edge(*candidate)
            candidate = MapGenerator.__select_removal_candidate(graph)

    @staticmethod
    def __scale_positions(graph, map_width, map_height):
        """
        :type graph: networkx.classes.graph.Graph
        """
        for node in graph.nodes_iter():
            pos = graph.node[node]["pos"]
            graph.node[node]["scaled_pos"] = (pos[0] * map_width, pos[1] * map_height)

    @staticmethod
    def __add_roadlengths(graph):
        """
        :type graph: networkx.classes.graph.Graph
        """
        for u, v in graph.edges_iter():
            MapGenerator.set_roadlength(graph, u, v)

    @staticmethod
    def generate_map(num_of_pois, num_of_plcs, num_of_crossings, max_x, max_y):
        """
        :type num_of_pois: int

        :rtype: networkx.classes.graph.Graph
        """
        g = nx.random_geometric_graph(num_of_pois + num_of_plcs + num_of_crossings, 0.3)

        # make connected
        components = nx.connected_components(g)
        for i in range(len(components)):
            for j in range(len(components)):
                if i != j:
                    n1 = random.choice(components[i])
                    n2 = random.choice(components[j])
                    if not g.has_edge(n1, n2):
                        g.add_edge(n1, n2)

        candidates = list(g.nodes())
        relable_mapping = dict()

        for i in range(num_of_pois):
            node = random.choice(candidates)
            g.node[node]["loctype"] = "poi"
            relable_mapping[node] = "poi" + str(i)
            candidates.remove(node)

        for i in range(num_of_plcs):
            node = random.choice(candidates)
            relable_mapping[node] = "pl" + str(i)
            g.node[node]["loctype"] = "plcs"
            candidates.remove(node)

        for i in range(num_of_crossings):
            node = random.choice(candidates)
            relable_mapping[node] = "c" + str(i)
            g.node[node]["loctype"] = "crossing"
            candidates.remove(node)
        nx.relabel_nodes(g, relable_mapping, copy=False)
        MapGenerator.__scale_positions(g, max_x, max_y)
        MapGenerator.__clean_graph(g)
        MapGenerator.__add_roadlengths(g)

        return g

    @staticmethod
    def load_from_graphml(path):
        """
        :type path: str
        :rtype: networkx.classes.graph.Graph
        """
        #: :type : networkx.classes.graph.Graph
        g1 = nx.read_graphml(path)
        #: :type : networkx.classes.graph.Graph
        g2 = nx.Graph()

        typetest = re.compile(r"([a-zA-z]+)(\d*)")

        max_qualifiers = dict(crossing=0, poi=0, plcs=0)
        node_mapping = dict()


        for node, data in g1.nodes_iter(data=True):

            m = typetest.match(data["label"])
            if m is None:
                raise(SALMAException("Wrong label format for node {}!".format(node)))

            loctype = m.group(1)
            if loctype in ["c"]:
                loctype = "crossing"
            elif loctype in ["p"]:
                loctype = "poi"
            elif loctype in ["pl"]:
                loctype = "plcs"
            if loctype not in ["poi", "plcs", "crossing"]:
                raise(SALMAException("Wrong loctype for node {}: {}".format(node, loctype)))
            qualifier = m.group(2)
            if len(qualifier) == 0:
                qualifier = max_qualifiers[loctype] + 1
                nid = data["label"] + str(qualifier)
            else:
                nid = data["label"]
            max_qualifiers[loctype] = max(max_qualifiers[loctype], qualifier)

            pos = (round(float(data["x"])), round(float(data["y"])))

            g2.add_node(nid, pos=pos, scaled_pos=pos, loctype=loctype)
            node_mapping[node] = nid

        for u, v in g1.edges_iter():
            n1 = node_mapping[u]
            n2 = node_mapping[v]
            g2.add_edge(n1, n2)

        MapGenerator.__add_roadlengths(g2)
        return g2

