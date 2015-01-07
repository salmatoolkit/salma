from salma.model.data import Term
from salma.model.procedure import *
from salma.model.process import TriggeredProcess


def create_plcs_processes(world, world_map, mt):
    process_reservation_requests = Procedure("main", [],
                                             [
                                                 Receive("reservation", "plcs", "reservation_requests"),
                                                 Iterate(Variable("reservation_requests"),
                                                         [Variable("req", "term")],
                                                         [
                                                             Assign("vehicle", "req.content.params[0]"),
                                                             If("availableSlots(self) > 0",
                                                                [
                                                                    Act("add_reservation",
                                                                        [Entity.SELF, Variable("vehicle"), 0, 0]),
                                                                    Send("reservation",
                                                                         Term("rresp", Entity.SELF, True, 0, 0),
                                                                         "plcs",
                                                                         Variable("vehicle"),
                                                                         "veh")
                                                                ],
                                                                [  # ELSE
                                                                   Send("reservation",
                                                                        Term("rresp", Entity.SELF, False, 0, 0),
                                                                        "plcs",
                                                                        Variable("vehicle"),
                                                                        "veh")
                                                                ])])])
    for plcs in world.getAgents("plcs"):
        p = TriggeredProcess(process_reservation_requests,
                             "message_available", [Entity.SELF, "reservation", "plcs"])
        plcs.add_process(p)
