from salma.model.agent import Agent
from salma.model.procedure import *
from salma.model.process import TriggeredProcess
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
        target = currentTargetPLCS(agent.id)
        r = nx.shortest_path(world_map, pos[1], target)
        return r

    def response_selector(agent=None, sam_responses=None, **ctx):
        """
        :type sam_responses: list[ReceivedMessage]
        :rtype: str
        """
        # for now: ignore any time information
        # format: rresp(StartTime, PlannedDuration, BestPLCS)
        if len(sam_responses) > 0:
            return sam_responses[0].content[3]
        else:
            return None

    return possible_target_chooser, route_finder, response_selector


def create_vehicles(world, world_map, mt, number_of_vehicles):
    target_chooser, route_finder, response_selector = create_navigation_functions(world_map, mt)

    p_request_plcs = Procedure("main", [],
                               [
                                   Assign("possible_targets",
                                          EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                          target_chooser, []),
                                   Send("assignment", ("areq", Entity.SELF, Variable("possible_targets"), 0, 0), "veh",
                                        "sam1", "sam"),
                                   SetFluent("waitingForAssignment", EvaluationContext.PYTHON_EXPRESSION, "True",
                                             [Entity.SELF])
                               ])

    p_make_reservation = Procedure("main", [],
                                   [
                                       Receive("assignment", "veh", "sam_responses"),
                                       Assign("chosen_plcs", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                              response_selector, []),
                                       Send("reservation", ("res", Entity.SELF, 0, 0), "veh", Variable("chosen_plcs"),
                                            "plcs")
                                   ])

    p_set_target = Procedure("main", [],
                             [
                                 Receive("reservation", "veh", "res_response"),
                                 SetFluent("waitingForAssignment", EvaluationContext.PYTHON_EXPRESSION, "False",
                                           [Entity.SELF]),
                                 If(EvaluationContext.PYTHON_EXPRESSION, "res_response[0].content[2] == True", [],
                                    [
                                        Assign("plcs", EvaluationContext.PYTHON_EXPRESSION,
                                               "res_response[0].content[1]", []),
                                        Act("setTargetPLCS", [Entity.SELF, Variable("plcs")])
                                    ])
                             ])

    p_find_route = Procedure("main", [],
                             [
                                 Assign("route", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                        route_finder, []),
                                 Act("setRoute", [Entity.SELF, Variable("route")])
                             ])

    for i in range(number_of_vehicles):
        p1 = TriggeredProcess(p_request_plcs, EvaluationContext.PYTHON_EXPRESSION,
                              "currentTargetPLCS(self) is None and "
                              "not waitingForAssignment(self)", [])
        # TODO: handle time-out for response from SAM

        p2 = TriggeredProcess(p_find_route, EvaluationContext.PYTHON_EXPRESSION,
                              "len(currentRoute(self)) == 0 and currentTargetPLCS(self) is not None", [])

        p3 = TriggeredProcess(p_set_target, EvaluationContext.PYTHON_EXPRESSION,
                              "len(local_channel_in_queue(self, 'reservation', 'veh')) > 0", [])

        p4 = TriggeredProcess(p_make_reservation, EvaluationContext.PYTHON_EXPRESSION,
                              "len(local_channel_in_queue(self, 'assignment', 'veh')) > 0", [])

        vehicle = Agent("v" + str(i), "vehicle", [p1, p2, p3, p4])
        world.addAgent(vehicle)
