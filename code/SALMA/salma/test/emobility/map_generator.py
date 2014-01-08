import networkx as nx
import random
import logging
import math

class MapGenerator(object):
    logger = logging.getLogger('salma')
    P_NODE_IS_STREET_END = 0.4

    def __init__(self):
        pass


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
        dist = math.ceil(random.uniform(direct_dist, 1.5*direct_dist))
        graph.edge[node1][node2]["weight"] = dist


    @staticmethod
    def __select_removal_candidate(graph):
        """
        :type graph: networkx.classes.graph.Graph
        :rtype: tuple
        """
        for u, v in graph.edges_iter():
            if nx.local_edge_connectivity(graph, u, v) > 1:
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



    def generate_map(self, num_of_pois, num_of_plcs, num_of_crossings, max_x, max_y):
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
            relable_mapping[node] = "plcs" + str(i)
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


