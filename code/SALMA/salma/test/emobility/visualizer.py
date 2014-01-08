import networkx as nx
import matplotlib.pyplot as plt

class Visualizer(object):

    def visualize_map(self, m):
        """
        :type m: networkx.classes.graph.Graph
        """
        positions = dict()
        for node, data in m.nodes_iter(data=True):
            positions[node] = data["scaled_pos"]

        pois = [n for n, data in m.nodes_iter(True) if data["loctype"] == "poi"]
        plcs = [n for n, data in m.nodes_iter(True) if data["loctype"] == "plcs"]
        crossings = [n for n, data in m.nodes_iter(True) if data["loctype"] == "crossing"]
        nx.draw_networkx_nodes(m, pos=positions, nodelist=pois, node_color='r')
        nx.draw_networkx_nodes(m, pos=positions, nodelist=plcs, node_color='g')
        nx.draw_networkx_nodes(m, pos=positions, nodelist=crossings, node_color='b')
        nx.draw_networkx_edges(m, positions)
        nx.draw_networkx_labels(m, positions)
        plt.show()

