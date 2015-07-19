from salma.constants import SELF
from salma.model.agent import Agent
from salma.model.procedure import Variable, Act, While, Wait, If
from salma.model.process import OneShotProcess


def create_robot(num):
    """
    Creates a simple agent that grabs an item with id item+num and keeps moving right as long as the agent is active
    """
    myItem = Variable("myItem")
    proc = OneShotProcess([
        Act("pickUp", [SELF, myItem]),
        While("xpos(self) < destX(myItem)", [
            Act("move_right", [SELF]),
            Wait("not moving(self)")
        ]),
        If("carrying(self, myItem)",
           Act("drop", [SELF, myItem]))
    ])
    agent = Agent("rob" + str(num), "robot", [proc], myItem="item" + str(num))
    return agent


    def initialize_robot(self, robot_id, x, y, vx, vy):
        self.world.setFluentValue("xpos", [robot_id], x)
        self.world.setFluentValue("ypos", [robot_id], y)
        self.world.setFluentValue("vx", [robot_id], vx)
        self.world.setFluentValue("vy", [robot_id], vy)
        self.world.setConstantValue("robot_radius", [robot_id], 1)
        self.world.setFluentValue("active", [robot_id], True)
        self.world.setFluentValue("partner", [robot_id], None)
        self.world.setFluentValue("grip", [robot_id], 0)
        items = self.world.getDomain('item')
        for i in items:
            self.world.setFluentValue('carrying', [robot_id, i.id], False)