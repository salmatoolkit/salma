from salma.model.data import Term
from salma.model.procedure import *
from salma.model.process import TriggeredProcess
from salma.constants import SELF


def create_plcs_processes(world, world_map, mt):
    vehicle, reservation_requests, req = makevars("vehicle", "reservation_requests", ("req", "term"))
    process_reservation_requests = Procedure([
        Receive("reservation", "plcs", "reservation_requests"),
        Iterate(reservation_requests, [req],
                [
                    Assign("vehicle", "req.content.params[0]"),
                    If("freeSlotsL(self) > 0",
                       [
                           Act("add_reservation",
                               [SELF, vehicle, 0, 0]),
                           Send("reservation",
                                Term("rresp", SELF, True, 0, 0),
                                "plcs",
                                vehicle,
                                "veh")
                       ],
                       [  # ELSE
                          Send("reservation",
                               Term("rresp", SELF, False, 0, 0),
                               "plcs",
                               vehicle,
                               "veh")
                       ])])])
    for plcs in world.getAgents("plcs"):
        p = TriggeredProcess(process_reservation_requests,
                             "message_available", [SELF, "reservation", "plcs"])
        plcs.add_process(p)
