import logging
import unittest

from salma import constants
from salma.SMCException import SMCException
from salma.engine import EclipseCLPEngine, EclipseCLPEngine
from salma.model import procedure, distributions
from salma.model.core import Entity, Fluent, Action
from salma.model.agent import Agent
import salma.model.process as prc

from salma.model.distributions import UniformDistribution, BernoulliDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    ActionExecution, Procedure, While, VariableAssignment, ArbitraryAction, Variable
from salma.model.world import World
from salma.test.testhelpers import withHeader

def printValue(value):
    print("Val: ", value)
    return ControlNode.CONTINUE, None


class WorldTest2(unittest.TestCase):
    """
    Conducts experiments with a simple scenario whose distribution can be given in closed form.
    """

    P_COLLISION = 0.9
    P_DROP = 0.02
    TIME_LIMIT = 20
    SAMPLE_LENGTH = 10
    SAMPLES = 10

    @classmethod
    def setUpClass(cls):
        try:
            World.set_logic_engine(EclipseCLPEngine("../../ecl-test/domaindesc.ecl",
                                                    "../../ecl-test/example_procedures.ecl"))
        except SMCException as e:
            print(e)
            raise
        logger = logging.getLogger('agamemnon-smc')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)

    def setUp(self):
        World.create_new_world()
        world = World.instance()
        world.load_declarations()
        world.get_exogenous_action(
            "accidental_drop").config.occurrence_distribution = BernoulliDistribution(WorldTest2.P_DROP)
        collision_event = world.get_exogenous_action("collision")
        collision_event.config.occurrence_distribution = BernoulliDistribution(WorldTest2.P_COLLISION)
        collision_event.config.uniform_param("severity", value_range=(0, 100))

    def __place_agents_in_column(self, x):
        world = World.instance()
        y = 10
        for r in world.getDomain('robot'):
            world.setFluentValue("xpos", [r.getId()], x)
            world.setFluentValue("ypos", [r.getId()], y)
            world.setFluentValue("active", [r.getId()], True)
            y += 20

    def create_right_moving_robot(self, num):
        """
        Creates a simple agent that grabs an item wit id item+num and keeps moving right as long as the agent is active
        """
        main_seq = Sequence()
        inner_seq = Sequence()
        inner_seq.addChild(ActionExecution("move_right", [Entity.SELF]))

        main_seq.addChild(ActionExecution("grab", [Entity.SELF, "item" + str(num)]))
        main_seq.addChild(While(EvaluationContext.PYTHON_EXPRESSION,
                                "True",
                                [Entity.SELF],
                                inner_seq))
        proc = prc.OneShotProcess(Procedure("main", [], main_seq))
        agent = Agent("rob" + str(num), "robot", [proc])
        return agent

    def set_no_one_carries_anything(self):
        world = World.instance()
        robots = world.getDomain('robot')
        items = world.getDomain('item')
        for r in robots:
            for i in items:
                world.setFluentValue('carrying', [r.id, i.id], False)

    @withHeader
    def testScenario(self):
        world = World.instance()  # : :type world: World

        for i in range(1, 3):
            world.addEntity(Entity("item" + str(i), "item"))
            world.addAgent(self.create_right_moving_robot(i))

        world.initialize(False)

        self.__place_agents_in_column(10)
        self.set_no_one_carries_anything()

        f_str = """
forall([r,robot],
    forall([i,item],
        implies(
            occur(grab(r,i)),
            until(25,
                carrying(r,i),
                xpos(r) > 20)
        )
)
)
"""
        world.registerProperty("f", f_str)

        #world.registerProperty("f", "forall([r,robot], xpos(r) > 1)")

        #world.registerProperty("f", "xpos(rob1) > 50")
        #world.registerProperty("f", "until(25, xpos(rob1) > 50")

        successes = []

        for i in range(0, WorldTest2.SAMPLES):
            res = world.runRepetitions(WorldTest2.SAMPLE_LENGTH)
            num = sum(res)
            successes.append(num)
            print("Run #{}: {}".format(i, num))


        print("Successes:")
        print(successes)


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
