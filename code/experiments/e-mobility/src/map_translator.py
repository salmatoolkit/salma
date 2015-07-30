import math
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.data import Term
from salma.model.world import World


class MapTranslator(object):

    def __init__(self, graph, world):
        """
        :type graph: networkx.classes.graph.Graph
        :type world: World
        """
        self.__graph = graph
        self.__world = world

    def init_world_from_graph(self):
        for node, data in self.__graph.nodes_iter(data=True):
            if self.__world.getEntityById(node) is None:
                if data["loctype"] == "poi":
                    self.__world.addEntity(Entity(node, "poi"))
                elif data["loctype"] == "plcs":
                    self.__world.addAgent(Agent(node, "plcs", world_declaration=self.__world))
                elif data["loctype"] == "crossing":
                    self.__world.addEntity(Entity(node, "crossing"))
        self.__world.set_constant_value("locX", ["sam1"], 500)
        self.__world.set_constant_value("locY", ["sam1"], 500)
        self.__world.set_constant_value("responsiblePLCSSAM", ["sam1"], "sam1")

        for node, data in self.__graph.nodes_iter(data=True):
            pos = data["scaled_pos"]
            self.__world.set_constant_value("locX", [node], pos[0])
            self.__world.set_constant_value("locY", [node], pos[1])
            self.__world.set_constant_value("responsiblePLCSSAM", [node], "sam1")
        road_num = 1
        for u, v, data in self.__graph.edges_iter(data=True):
            road_id = "r{}".format(road_num)
            road_num += 1
            self.__world.addEntity(Entity(road_id, "road"))
            self.__world.set_constant_value("roadEnds", [road_id], Term("r", u, v))
            self.__world.set_constant_value("roadlength", [road_id], data["weight"])
            self.__world.set_constant_value("roadBaseSpeed", [road_id], 10)
            self.__graph.edge[u][v]["road_id"] = road_id

    def get_position_from_node(self, node):
        """
        :type node: str
        :rtype: (int, int)
        """
        pos = self.__graph.node[node]["scaled_pos"]
        return pos[0], pos[1]

    def find_closest_node(self, x, y, loctype=None):
        """
        :type x: int
        :type y: int
        :type loctype: str
        :rtype: str
        """
        min_distance = None
        closest_node = None
        for loc, data in self.__graph.nodes_iter(True):
            if loctype is None or data["loctype"] == loctype:
                loc_x = data["scaled_pos"][0]
                loc_y = data["scaled_pos"][1]
                distance = math.sqrt((x - loc_x)**2 + (y - loc_y)**2)
                if min_distance is None or distance < min_distance:
                    min_distance = distance
                    closest_node = loc
        return closest_node

    def find_k_closest_nodes(self, x, y, k, loctype=None):
        """
        :type x: int
        :type y: int
        :type k: int
        :type loctype: str
        :rtype: list(str)
        """
        nodes_by_distance = []
        for loc, data in self.__graph.nodes_iter(True):
            if loctype is None or data["loctype"] == loctype:
                loc_x = data["scaled_pos"][0]
                loc_y = data["scaled_pos"][1]
                distance = math.sqrt((x - loc_x)**2 + (y - loc_y)**2)
                nodes_by_distance.append((loc, distance))
        sorted_nodes = sorted(nodes_by_distance, key=lambda n: n[1])
        return list(map(lambda n: n[0], sorted_nodes[:k]))
