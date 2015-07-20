from salma.constants import SELF
from salma.model.agent import Agent
from salma.model.procedure import Variable, Act, While, Wait, If, Assign, makevars, Procedure, Iterate, Select
from salma.model.process import OneShotProcess, PeriodicProcess, TriggeredProcess


def create_robot(num):
    """
    Creates a simple agent that grabs an item with id item+num and keeps moving right as long as the agent is active
    """
    target, tx, ty = makevars("target", "tx", "ty")
    p = Procedure([
        While("True", [
            Wait("next_task(self) is not None"),
            Assign(target, "next_task(self)"),
            Assign(tx, "xpos(next_task(self))"),
            Assign(ty, "ypos(next_task(self))"),
            While("(xpos(self) != tx) or "
                  "(ypos(self) != ty)", [
                      If("xpos(self) < tx",
                         Act("move_right", [SELF]),
                         If("xpos(self) > tx",
                            Act("move_left", [SELF]))),
                      Wait("not moving(self)"),
                      If("ypos(self) < ty",
                         Act("move_down", [SELF]),
                         If("ypos(self) > ty",
                            Act("move_up", [SELF]))),
                      Wait("not moving(self)")]),
            Act("grab", [SELF, target]),
            #
            While("ypos(self) != stationY(base)", [
                If("ypos(self) < stationY(base)",
                   Act("move_down", [SELF]),
                   If("ypos(self) > stationY(base)",
                      Act("move_up", [SELF]))),
                Wait("not moving(self)")]),
            While("xpos(self) != stationX(base)", [
                If("xpos(self) < stationX(base)",
                   Act("move_right", [SELF]),
                   If("xpos(self) > stationX(base)",
                      Act("move_left", [SELF]))),
                Wait("not moving(self)")]),
            Act("deliver", [SELF, target, "base"])
        ])])
    proc = OneShotProcess(p)
    agent = Agent("rob" + str(num), "robot", [proc])
    return agent


def create_base():
    r, i = makevars(("r", "robot"), ("i", "item"))
    p = Procedure([
        Iterate("idle", [r], [
            Select("undelivered", [i]),
            If("i is not None",
               Act("assign_task", [SELF, r, i]))])
    ])
    proc = PeriodicProcess(p, 50)

    return Agent("base", "station", [proc])
