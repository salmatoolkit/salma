import networkx as nx
from salma.model.core import Entity
from salma.model.world import World

class MapTranslator(object):

    def init_world_from_graph(self, graph, world):
        """
        :type graph: networkx.classes.graph.Graph
        :type world: World
        """
        for node, data in graph.nodes_iter(data=True):
            if data["loctype"] == "poi":
                world.addEntity(Entity(node, "poi"))
            elif data["loctype"] == "plcs":
                world.addEntity(Entity(node, "plcs"))
            elif data["loctype"] == "crossing":
                world.addEntity(Entity(node, "crossing"))
        world.addEntity(Entity("sam1","plcssam"))
        world.initialize(False)

        for node, data in graph.nodes_iter(data=True):
            pos = data["position"]
            world.setConstantValue("locX",[node],pos[0])
            world.setConstantValue("locY",[node],pos[1])
            world.setConstantValue("responsiblePLCSSAM", [node], "sam1")

        for u, v, data in graph.edges_iter(data=True):
            world.setConstantValue("connected", [u, v], True)
            world.setConstantValue("roadlength", [u, v], data["weight"])


