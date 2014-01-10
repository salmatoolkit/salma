import networkx as nx
import matplotlib.pyplot as plt
from salma.model.world import World

class Visualizer(object):

    def visualize_map(self, m, world):
        """
        :type m: networkx.classes.graph.Graph
        :type world: World
        """
        positions = dict()
        for node, data in m.nodes_iter(data=True):
            positions[node] = data["scaled_pos"]

        pois = [n for n, data in m.nodes_iter(True) if data["loctype"] == "poi"]
        plcs = [n for n, data in m.nodes_iter(True) if data["loctype"] == "plcs"]
        crossings = [n for n, data in m.nodes_iter(True) if data["loctype"] == "crossing"]

        #: :type : dict of (str, list)
        nodes_with_vehicles = dict()
        #: :type : dict of ((str, str), list)
        edges_with_vehicles = dict()

        for vehicle in world.getDomain("vehicle"):
            pos = world.getFluentValue("vehiclePosition", [vehicle.id])
            if pos[1] == pos[2]:
                if pos[1] not in nodes_with_vehicles:
                    nodes_with_vehicles[pos[1]] = []
                nodes_with_vehicles[pos[1]].append(vehicle.id)
            else:
                edge = pos[1], pos[2]
                if edge not in edges_with_vehicles:
                    edges_with_vehicles[edge] = []
                edges_with_vehicles[edge].append(vehicle.id)

        node_colors = []
        edge_colors = []
        for node, data in m.nodes_iter(True):
            if node in nodes_with_vehicles:
                node_colors.append("r")
            else:
                node_colors.append("b")

        for u, v, data in m.edges_iter(data=True):
            if (u,v) in edges_with_vehicles:
                edge_colors.append("r")
            else:
                edge_colors.append("k")

        nx.draw_networkx_nodes(m, pos=positions, node_color=node_colors)
        nx.draw_networkx_edges(m, positions, edge_color=edge_colors)
        nx.draw_networkx_labels(m, positions)



