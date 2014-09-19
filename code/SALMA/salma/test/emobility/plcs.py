from salma.model.procedure import *
from salma.model.process import PeriodicProcess, TriggeredProcess


def create_plcs_processes(world, world_map, mt):

    process_reservation_requests = Procedure("main", [],
                                             [
                                                 Receive("reservation", "plcs", "reservation_requests"),
                                                 Iterate(EvaluationContext.ITERATOR,
                                                         Variable("reservation_requests"),
                                                         [("req", "term")],
                                                         [
                                                             Assign("vehicle", EvaluationContext.PYTHON_EXPRESSION,
                                                                    "req.content[1]", []),
                                                             If(EvaluationContext.PYTHON_EXPRESSION,
                                                                "avaliableSlots(self) > 0", [],
                                                                [
                                                                    Act("add_reservation",
                                                                        [Entity.SELF, Variable("vehicle"), 0, 0]),
                                                                    Send("reservation",
                                                                         ("rresp", Entity.SELF, True, 0, 0), "plcs",
                                                                         Variable("vehicle"),
                                                                         "veh")
                                                                ],
                                                                elseBody=[
                                                                    Send("reservation",
                                                                         ("rresp", Entity.SELF, False, 0, 0), "plcs",
                                                                         Variable("vehicle"),
                                                                         "veh")

                                                                ])
                                                         ])
                                             ])
    for plcs in world.getAgents("plcs"):
        p = TriggeredProcess(process_reservation_requests, EvaluationContext.TRANSIENT_FLUENT,
                              "message_available", [Entity.SELF, "reservation", "plcs"])
        plcs.add_process(p)
