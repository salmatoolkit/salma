from salma.constants import SELF
from salma.model.agent import Agent
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Variable, Act, While, Wait, If, Assign, makevars, Procedure, Iterate, Select, Switch, \
    Case
from salma.model.process import OneShotProcess, PeriodicProcess, TriggeredProcess
import numpy as np


def create_move_loop():
    return While("not self.broken and"
                 "(next_task(self) != None) and "
                 "((xpos(self) != tx) or (ypos(self) != ty))", [
                     Switch(
                         Case("self.xpos < tx", Act("move_right", [SELF])),
                         Case("self.xpos > tx", Act("move_left", [SELF]))
                     ),
                     Wait("ready(self)"),
                     Switch(
                         Case("self.ypos < ty", Act("move_down", [SELF])),
                         Case("self.ypos > ty", Act("move_up", [SELF]))
                     ),
                     Wait("ready(self)")])


def create_robot(num):
    """
    Creates a simple agent that grabs an item with id item+num and keeps moving right as long as the agent is active
    """
    targetItem, targetWs, tx, ty = makevars("targetItem", "targetWs", "tx", "ty")

    p = Procedure([
        While("not broken(self)", [
            Wait("next_task(self) != None"),
            Assign(targetItem, "task_item", [SELF]),
            Assign(targetWs, "task_workstation", [SELF]),
            Assign(tx, "xpos(targetItem)"),
            Assign(ty, "ypos(targetItem)"),
            create_move_loop(),
            If("not self.broken",
               Act("grab", [SELF, targetItem])),
            Assign(tx, "stationX(targetWs)"),
            Assign(ty, "stationY(targetWs)"),
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


def create_coordinator():
    r, i, ws = makevars(("r", "robot"), ("i", "item"), ("ws", "workstation"))
    p = Procedure([
        Iterate("request_queue(self)", [ws], [
            Select("unassigned", [r]),
            Assign(i, select_item, [r]),
            If("i != None and r != None",
               Act("assign_task", [SELF, r, i, ws]))])
    ])
    proc = PeriodicProcess(p, 50)
    return Agent("coordinator1", "coordinator", [proc])
