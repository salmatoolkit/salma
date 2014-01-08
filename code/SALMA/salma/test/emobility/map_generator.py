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
    def generate_position(blocked_positions, max_x, max_y):
        """
        :type blocked_positions: list of (int,int)
        :rtype: (int, int)
        """
        if len(blocked_positions) > (max_x+1)*(max_y+1)*0.8:
            raise BaseException("Too many positions blocked!")
        misses = 0
        while True:
            x = random.randint(0, max_x)
            y = random.randint(0, max_y)

            if (x,y) not in blocked_positions:
                MapGenerator.logger.debug("Generated position {}. Misses: {}".format((x,y),misses))
                return (x,y)
            else:
                misses += 1

    @staticmethod
    def node_is_full(graph, node, node_data):
        """
        :type graph: networkx.classes.graph.Graph
        :typen node: str
        :type node_data: dict
        :rtype: bool
        """
        if node_data["loctype"] == "crossing":
            return graph.degree(node) >= 4
        else:
            return graph.degree(node) >= 2

    @staticmethod
    def choose_neighbor(graph, node):
        """
        :type graph: networkx.classes.graph.Graph
        :type node: str
        :rtype: str
        """
        candidates = set(graph.nodes()) - {node}

        while len(candidates) > 0:
            act_candidate = random.choice(list(candidates))
            if (MapGenerator.node_is_full(graph, act_candidate, graph.node[act_candidate])
                    or graph.has_edge(node, act_candidate)
                    # threshold for distance. does this make sense?
                    #or MapGenerator.get_dist(graph, node, act_candidate) > 400
                    ):
                        candidates -= {act_candidate}
            else:
                return act_candidate

        return None

    @staticmethod
    def get_dist(graph, node1, node2):
        pos1 = graph.node[node1]["position"]
        pos2 = graph.node[node2]["position"]
        return math.sqrt((pos2[0]-pos1[0])**2 + (pos2[1]-pos1[1])**2)

    @staticmethod
    def add_road(graph, node1, node2):
        """
        :type graph: networkx.classes.graph.Graph
        :type node1: str
        :type node2: str
        """
        direct_dist = MapGenerator.get_dist(graph, node1, node2)
        dist = math.ceil(random.uniform(direct_dist, 1.5*direct_dist))
        graph.add_edge(node1, node2, weight=dist)


    def generate_map(self, num_of_pois, num_of_plcs, num_of_crossings, max_x, max_y):
        """
        :type num_of_pois: int

        :rtype: networkx.classes.graph.Graph
        """
        g = nx.Graph()
        positions = []
        # add pois

        for i in range(num_of_pois):
            pos = MapGenerator.generate_position(positions, max_x, max_y)
            positions.append(pos)
            g.add_node("poi" + str(i), loctype="poi", position=pos)

        for i in range(num_of_plcs):
            pos = MapGenerator.generate_position(positions, max_x, max_y)
            positions.append(pos)
            g.add_node("plcs" + str(i), loctype="plcs", position=pos)

        for i in range(num_of_crossings):
            pos = MapGenerator.generate_position(positions, max_x, max_y)
            positions.append(pos)
            g.add_node("crossing" + str(i), loctype="crossing", position=pos)

        for node, data in g.nodes_iter(True):
            if data["loctype"] != "crossing" and not MapGenerator.node_is_full(g, node, data):

                neighbor_num = 2 if random.uniform(0,1) <= (1-MapGenerator.P_NODE_IS_STREET_END) else 1
                neighbors_to_add = neighbor_num - g.degree(node)
                for i in range(neighbors_to_add):
                    neighbor = MapGenerator.choose_neighbor(g, node)
                    if neighbor is not None:
                        MapGenerator.add_road(g, node, neighbor)

        for node, data in g.nodes_iter(True):
            if data["loctype"] == "crossing" and not MapGenerator.node_is_full(g, node, data):
                neighbors_total = 4 if random.uniform(0,1) <= 0.5 else 3
                neighbors_to_add = neighbors_total - g.degree(node)
                for i in range(neighbors_to_add):
                    neighbor = MapGenerator.choose_neighbor(g, node)
                    if neighbor is not None:
                        MapGenerator.add_road(g, node, neighbor)
        return g


