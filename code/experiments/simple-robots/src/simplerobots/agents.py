from salma.constants import SELF
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Variable, Act, While, Wait, If, Assign, makevars, Procedure, Iterate, Select, Switch, \
    Case
from salma.model.process import OneShotProcess, PeriodicProcess, TriggeredProcess
import numpy as np


def create_move_loop():
    return While("not self.broken and "
                 "self.next_task != None and "
                 "(self.xpos != tx or self.ypos != ty)", [
                     Switch(
                         Case("self.xpos < tx", Act("move_right", [SELF])),
                         Case("self.xpos > tx", Act("move_left", [SELF]))
                     ),
                     Wait("self.ready"),
                     Switch(
                         Case("self.ypos < ty", Act("move_down", [SELF])),
                         Case("self.ypos > ty", Act("move_up", [SELF]))
                     ),
                     Wait("self.ready")])


def create_robot(num):
    targetItem, targetWs, tx, ty = makevars("targetItem", "targetWs", "tx", "ty")

    p = Procedure([
        While("not self.broken", [
            Wait("self.next_task != None"),
            Assign(targetItem, "task_item", [SELF]),
            Assign(targetWs, "task_workstation", [SELF]),
            Assign(tx, "targetItem.xpos"),
            Assign(ty, "targetItem.ypos"),
            create_move_loop(),
            If("not self.broken",
               Act("pickUp", [SELF, targetItem])),
            Assign(tx, "targetWs.stationX"),
            Assign(ty, "targetWs.stationY"),
            create_move_loop(),
            If("not self.broken and "
               "dist_from_station(self, targetWs) == 0 and carrying(self, targetItem)",
               Act("deliver", [SELF, targetItem, targetWs]))
        ])])
    proc = OneShotProcess(p)
    agent = Agent("rob" + str(num), "robot", [proc])
    return agent


def select_item(rob: Agent, ctx: EvaluationContext=None, **kwargs):
    if rob is None:
        return None
    closest_item = None
    min_dist = None
    for item in ctx.getDomain("item"):
        if item.undelivered:
            dist = np.sqrt((rob.xpos - item.xpos) ** 2 + (rob.ypos - item.ypos) ** 2)
            if min_dist is None or dist < min_dist:
                closest_item = item
                min_dist = dist
    return closest_item


def select_item2(station: Entity, ctx: EvaluationContext=None, **kwargs):
    # determine closest robot
    dist = lambda r: np.sqrt((r.xpos - station.stationX) ** 2 + (r.ypos - station.stationY) ** 2)

    robot_distances = [(dist(r), r) for r in ctx.getDomain("robot") if r.unassigned]
    if len(robot_distances) == 0:
        return None, None
    closest_robot = min(robot_distances)[1]

    dist = lambda i: np.sqrt((i.xpos - closest_robot.xpos) ** 2 + (i.ypos - closest_robot.ypos) ** 2)
    item_distances = [(dist(i), i) for i in ctx.getDomain("item") if i.undelivered]
    if len(item_distances) == 0:
        return None, None
    closest_item = min(item_distances)[1]
    return closest_robot, closest_item


def create_coordinator():
    r, i, ws = makevars(("r", "robot"), ("i", "item"), ("ws", "workstation"))
    p = Procedure([
        Iterate("self.request_queue", [ws], [
            Select("unassigned", [r]),
            Assign(i, select_item, [r]),
            If("i != None and r != None",
               Act("assign_task", [SELF, r, i, ws]))])
    ])
    proc = PeriodicProcess(p, 50)
    return Agent("coordinator1", "coordinator", [proc])


def create_coordinator_clever():
    r, i, ws = makevars(("r", "robot"), ("i", "item"), ("ws", "workstation"))
    p = Procedure([
        Iterate("self.request_queue", [ws], [
            Assign((r, i), select_item2, [ws]),
            If("i != None and r != None",
               Act("assign_task", [SELF, r, i, ws]))])
    ])
    proc = PeriodicProcess(p, 50)
    return Agent("coordinator1", "coordinator", [proc])
