from salma.model.agent import Agent
from salma.model.procedure import *
from salma.model.process import TriggeredProcess
from salma.model.infotransfer import ReceivedMessage


def create_plcssam_functions(world_map, mt):
    def process_assignment_requests(agent=None, assignment_requests=None, freeSlotsR=None, **ctx):
        """
        :type agent: salma.model.agent.Agent
        :type assignment_requests: list[ReceivedMessage]
        :rtype: list[(str,str)]
        """
        # : :type : EvaluationContext
        ec = agent.evaluation_context

        # format: rreq(Vehicle, Alternatives, StartTime, PlannedDuration)
        plcsdom = ec.getDomain("plcs")

        free_slot_map = dict()
        for plcs in plcsdom:
            fs = freeSlotsR(agent.id, plcs.id)
            free_slot_map[plcs.id] = fs

        result = []
        for r in assignment_requests:
            vehicle = r.content[1]
            #: :type: list
            alternatives = r.content[2]
            for plcs in alternatives:
                fs = (free_slot_map[plcs] if plcs in free_slot_map
                      else freeSlotsR(agent.id, plcs))
                if fs is None:
                    fs = 0
                if fs > 0:
                    result.append((vehicle, plcs))
                    fs -= 1
                free_slot_map[plcs] = fs
        # todo: solve constraint
        return result

    return process_assignment_requests


def create_plcssam(world, world_map, mt):
    request_processor = create_plcssam_functions(world_map, mt)
    assignments, p, v = makevars("assignments", ("p", "plcs"), ("v", "vehicle"))
    p_process_requests = Procedure("main", [],
                                   [
                                       Receive("assignment", "sam", "assignment_requests"),
                                       Assign(assignments, request_processor),
                                       Iterate(Variable("assignments"),
                                               [v, p],
                                               Send("assignment", ("aresp", 0, 0, p), "sam", v, "veh"))])

    p1 = TriggeredProcess(p_process_requests, "message_available", [Entity.SELF, "assignment", "sam"])

    sam = Agent("sam1", "plcssam", [p1], world_declaration=world)
    world.addAgent(sam)