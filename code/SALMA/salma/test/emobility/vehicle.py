from networkx.classes.graph import Graph
from salma.model.agent import Agent
from salma.model.data import Term
from salma.model.procedure import *
from salma.model.process import TriggeredProcess, PeriodicProcess
import networkx as nx
from salma.model.infotransfer import ReceivedMessage


def create_navigation_functions(world_map, mt):
    def possible_target_chooser(agent=None, currentTargetPOI=None, **ctx):
        target_poi = currentTargetPOI(agent.id)
        x, y = mt.get_position_from_node(target_poi)
        target = mt.find_k_closest_nodes(x, y, 3, loctype="plcs")
        return target

    def route_finder(agent=None, vehiclePosition=None, currentTargetPLCS=None, **ctx):
        pos = vehiclePosition(agent.id)
        assert isinstance(pos, Term)
        assert pos.functor == "l"
        target = currentTargetPLCS(agent.id)
        r = nx.shortest_path(world_map, pos.params[0], target)
        assert isinstance(world_map, Graph)
        if len(r) <= 1:
            return []
        roads = []
        for i in range(len(r) - 1):
            road_id = world_map.edge[r[i]][r[i+1]]["road_id"]
            roads.append(road_id)
        return roads

    def response_selector(agent=None, sam_responses=None, **ctx):
        """
        :type sam_responses: list[ReceivedMessage]
        :rtype: str
        """
        # for now: ignore any time information
        # format: rresp(StartTime, PlannedDuration, BestPLCS)
        if len(sam_responses) > 0:
            msg = sam_responses[0].content
            assert isinstance(msg, Term)
            return msg.params[2]
        else:
            return None

    return possible_target_chooser, route_finder, response_selector


def create_vehicles(world, world_map, mt, number_of_vehicles):
    target_chooser, route_finder, response_selector = create_navigation_functions(world_map, mt)

    p_request_plcs = Procedure("main", [],
                               [
                                   If("currentTargetPLCS(self) is None",
                                      [
                                          Assign("possible_targets", target_chooser),
                                          Send("assignment", 
                                               Term("areq", Entity.SELF, Variable("possible_targets"), 0, 0),
                                               "veh", "sam1", "sam")
                                      ])])

    p_make_reservation = Procedure("main", [],
                                   [
                                       Receive("assignment", "veh", "sam_responses"),
                                       Assign("chosen_plcs", response_selector),
                                       Send("reservation",
                                            Term("res", Entity.SELF, 0, 0), "veh", Variable("chosen_plcs"),
                                            "plcs")])

    p_set_target = Procedure("main", [],
                             [
                                 Receive("reservation", "veh", "res_response"),
                                 Act("setWaitingForAssignment", [Entity.SELF, False]),
                                 If("res_response[0].content.params[1] == True",
                                    [
                                        Assign("plcs", "res_response[0].content.params[0]"),
                                        Act("setTargetPLCS", [Entity.SELF, Variable("plcs")])])])

    p_find_route = Procedure("main", [],
                             [
                                 Assign("route", route_finder),
                                 Act("setRoute", [Entity.SELF, Variable("route")])])

    for i in range(number_of_vehicles):
        p1 = PeriodicProcess(p_request_plcs, 10)
        p2 = TriggeredProcess(p_find_route,
                              "len(currentRoute(self)) == 0 and currentTargetPLCS(self) is not None")

        p3 = TriggeredProcess(p_set_target,
                              "len(local_channel_in_queue(self, 'reservation', 'veh')) > 0 "
                              "and currentTargetPLCS(self) is None")

        p4 = TriggeredProcess(p_make_reservation,
                              "currentTargetPLCS(self) is None and "
                              "len(local_channel_in_queue(self, 'assignment', 'veh')) > 0")

        vehicle = Agent("v" + str(i), "vehicle", [p1, p2, p3, p4])
        world.addAgent(vehicle)