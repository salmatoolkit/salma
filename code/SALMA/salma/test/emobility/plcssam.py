from salma.model.agent import Agent
from salma.model.procedure import *
from salma.model.process import TriggeredProcess
from salma.test.emobility import utils
from salma.model.infotransfer import ReceivedMessage


def create_plcssam_functions(world_map, mt):
    def process_assignment_requests(agent=None, assignment_requests=None, **ctx):
        """
        :type agent: salma.model.agent.Agent
        :type assignment_requests: list[ReceivedMessage]
        :rtype: list[(str,str)]
        """
        # : :type : EvaluationContext
        ec = agent.evaluation_context

        # format: rreq(Vehicle, Alternatives, StartTime, PlannedDuration)
        schedule = []
        assignment = dict()
        for r in assignment_requests:
            schedule.append((r.content[1], r.content[2]))  # remember that position 0 is the "message envelope" "rreq"
        success = utils.choose_alternative(schedule, assignment)
        if not success:
            return []
        result = []
        for vehicle, plcs in assignment.items():
            result.append((vehicle, plcs))

        # todo: establish communication between SAM and PLCs to check availability
        # TODO: consider time slots
        return result

    return process_assignment_requests


def create_plcssam(world, world_map, mt):
    request_processor = create_plcssam_functions(world_map, mt)

    p_process_requests = Procedure("main", [],
                                   [

                                       Receive("assignment", "sam", "assignment_requests"),
                                       Assign("assignments", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                                              request_processor, []),
                                       Iterate(EvaluationContext.ITERATOR, Variable("assignments"),
                                               [("v", "vehicle"), ("p", "plcs")],
                                               Send("assignment", ("aresp", 0, 0, Variable("p")), "sam", Variable("v"),
                                                    "veh"))
                                   ])

    # p_free_slots_receiver = Procedure("main", [],
    #                                   [
    #                                       Receive("freeSlotsR", "freeSlotsR", "free_slot_msgs"),
    #                                       Assign("freeSlotsMap", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
    #                                              free_slots_receiver, []),
    #                                       Iterate(EvaluationContext.ITERATOR, Variable("freeSlotsMap"),
    #                                               [("p", "plcs"), ("fs", "integer")],
    #                                               SetFluent("freeSlotsR", EvaluationContext.PYTHON_EXPRESSION, "fs",
    #                                                         [Entity.SELF, Variable("p")]))
    #                                   ])
    p1 = TriggeredProcess(p_process_requests, EvaluationContext.TRANSIENT_FLUENT,
                          "message_available", [Entity.SELF, "assignment", "sam"])

    sam = Agent("sam1", "plcssam", [p1], world_declaration=world)
    world.addAgent(sam)