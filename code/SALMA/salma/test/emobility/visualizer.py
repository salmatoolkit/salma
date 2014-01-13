import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from salma.model.world import World
import numpy as np


def create_label_list(ids : list) -> str:
    return ", ".join(map(str, ids))

class Visualizer(object):
    def __init__(self, worldmap, world):
        """
        :type worldmap: networkx.classes.graph.Graph
        :type world: World
        """
        self.worldmap = worldmap
        self.world = world

        max_x, max_y = 0, 0

        for node, data in self.worldmap.nodes_iter(True):
            x, y = data["scaled_pos"]
            max_x = max(max_x, x)
            max_y = max(max_y, y)

        self.limits = (max_x + 20, max_y + 20)



    def visualize_map(self, fig):
        """
        :type m: networkx.classes.graph.Graph
        :type fig: Figure
        :type world: World
        """
        m = self.worldmap
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

        for vehicle in self.world.getDomain("vehicle"):
            pos = self.world.getFluentValue("vehiclePosition", [vehicle.id])
            if pos[1] == pos[2]:
                if pos[1] not in nodes_with_vehicles:
                    nodes_with_vehicles[pos[1]] = []
                nodes_with_vehicles[pos[1]].append(vehicle.id)
            else:
                edge = pos[1], pos[2]
                rev_edge = pos[2], pos[1]
                if edge not in edges_with_vehicles:
                    edges_with_vehicles[edge] = []
                if rev_edge not in edges_with_vehicles:
                    edges_with_vehicles[rev_edge] = []
                edges_with_vehicles[edge].append(vehicle.id)
                edges_with_vehicles[rev_edge].append(vehicle.id)

        node_colors = []
        edge_colors = []

        for node, data in m.nodes_iter(True):
            if node in nodes_with_vehicles:
                node_colors.append("r")
            else:
                node_colors.append("b")
        edge_labels = dict()
        for u, v, data in m.edges_iter(data=True):
            if (u, v) in edges_with_vehicles:
                edge_labels[(u, v)] = create_label_list(edges_with_vehicles[(u, v)])
                edge_labels[(v, u)] = create_label_list(edges_with_vehicles[(u, v)])
                edge_colors.append("r")
            elif (v, u) in edges_with_vehicles:
                edge_labels[(u, v)] = create_label_list(edges_with_vehicles[(v, u)])
                edge_labels[(v, u)] = create_label_list(edges_with_vehicles[(v, u)])
                edge_colors.append("r")
            else:
                edge_colors.append("k")




        #: :type : Axes
        ax = fig.gca()
        ax.set_xlim(0, self.limits[0])
        ax.set_ylim(self.limits[1], 0)
        nx.draw_networkx_nodes(m, pos=positions, node_color=node_colors, ax=ax)
        nx.draw_networkx_edges(m, positions, edge_color=edge_colors, ax=ax)
        nx.draw_networkx_labels(m, positions, ax=ax)
        nx.draw_networkx_edge_labels(m, pos=positions, edge_labels=edge_labels, ax=ax)
        # for node, vehicles in nodes_with_vehicles.items():
        #     x, y = m.node[node]["scaled_pos"]
        #     label = str(vehicles)
        #     ax.annotate(label, xy=(x, y), fontsize=9)

        poslabels = []
        vids = [v.id for v in self.world.getDomain("vehicle")]

        for vid in sorted(vids):
            pos = self.world.getFluentValue("vehiclePosition", [vid])
            poslabels.append("{}:({},{},{})".format(vid, pos[1], pos[2], pos[3]))
        time = self.world.getTime()
        poslabel = ", ".join(poslabels)
        ax.text(20, 20, "t={:06}, {}".format(time, poslabel), fontsize=9)