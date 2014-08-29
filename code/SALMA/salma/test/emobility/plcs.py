from salma.model.procedure import *
from salma.model.process import PeriodicProcess, TriggeredProcess


def create_plcs_processes(world, world_map, mt):
    sense_slots = Procedure("main", [],
                            [
                                Sense("freeSlotsL", [])
                            ])
    send_freeSlots = Procedure("main", [],
                               [
                                   If(EvaluationContext.PYTHON_EXPRESSION, "freeSlotsL(self) is not None", [],
                                      [
                                          Assign("sensorValue", EvaluationContext.FLUENT, "freeSlotsL",
                                                 [Entity.SELF]),
                                          Send("chan_freeSlotsR", ("rs", Entity.SELF, Variable("sensorValue")), "plcs",
                                               "sam1", "sam")
                                      ])
                               ])

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
        p1 = PeriodicProcess(sense_slots, 5)
        p2 = PeriodicProcess(send_freeSlots, 5)
        p3 = TriggeredProcess(process_reservation_requests, EvaluationContext.PYTHON_EXPRESSION,
                              "len(local_channel_in_queue(self, 'reservation', 'plcs')) > 0", [])
        plcs.add_process(p1)
        plcs.add_process(p2)
        plcs.add_process(p3)
