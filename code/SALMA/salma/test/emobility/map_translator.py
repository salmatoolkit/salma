import math
import networkx as nx
from salma.model.core import Entity
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
        """
        :type graph: networkx.classes.graph.Graph
        :type world: World
        """
        for node, data in self.__graph.nodes_iter(data=True):
            if data["loctype"] == "poi":
                self.__world.addEntity(Entity(node, "poi"))
            elif data["loctype"] == "plcs":
                self.__world.addEntity(Entity(node, "plcs"))
            elif data["loctype"] == "crossing":
                self.__world.addEntity(Entity(node, "crossing"))
        self.__world.addEntity(Entity("sam1", "plcssam"))
        self.__world.initialize(False)
        self.__world.setConstantValue("locX", ["sam1"], 500)
        self.__world.setConstantValue("locY", ["sam1"], 500)
        self.__world.setConstantValue("responsiblePLCSSAM", ["sam1"], "sam1")

        for node, data in self.__graph.nodes_iter(data=True):
            pos = data["scaled_pos"]
            self.__world.setConstantValue("locX", [node], pos[0])
            self.__world.setConstantValue("locY", [node], pos[1])
            self.__world.setConstantValue("responsiblePLCSSAM", [node], "sam1")

        for u, v, data in self.__graph.edges_iter(data=True):
            self.__world.setConstantValue("connected", [u, v], True)
            self.__world.setConstantValue("roadlength", [u, v], data["weight"])

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
