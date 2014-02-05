import logging
import pprint
import unittest
from scipy.odr.__odrpack import odr_stop

from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions
from salma.model.core import Agent, Entity, Fluent, Action, Constant
from salma.model.actions import StochasticAction, DeterministicAction
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    ActionExecution, Procedure, While, VariableAssignment, ArbitraryAction, Variable, \
    Iterate, SelectFirst, ProcedureRegistry, ProcedureCall, If, Plan
from salma.model.world import World
from salma.test.testhelpers import withHeader
import os

# TODO: integrate proper assertions!
def printValue(value):
    print("Val: ", value)
    return (ControlNode.CONTINUE, None)


class WorldTest3(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        print(os.getcwd())
        try:
            World.set_logic_engine(EclipseCLPEngine("../../ecl-test/domaindesc.ecl",
                                                  "../../ecl-test/example_procedures.ecl"))
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('agamemnon-smc')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)

    def setUp(self):
        World.create_new_world()
        world = World.instance()


    def test_load_declaration_empty(self):
        world = World.instance()
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  ActionExecution("move_right", [Entity.SELF]))
        proc = Procedure("main", [], w)

        agent1 = Agent("rob1", "robot", proc)
        world.addAgent(agent1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.load_declarations()
        world.initialize(False)
        fl, const = world.check_fluent_initialization()
        print(fl)
        print(const)
        self.assertEqual(len(fl), 7)
        self.assertEqual(len(const), 2)
        a, a2 = world.check_action_initialization()
        print("---------")
        for act in a:
            print(str(act[0]) + " : " + str(act[1]))
        for act in a2:
            print(str(act[0]) + " : " + str(act[1]))
        self.assertEqual(len(a), 1)
        self.assertEqual(len(a2), 2)
        print("----")
        print(world.getAllActions())
        self.assertEqual(len(list(world.getAllActions())), 12)


    def test_load_declaration_full(self):
        world = World.instance()
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  ActionExecution("move_right", [Entity.SELF]))
        proc = Procedure("main", [], w)

        agent1 = Agent("rob1", "robot", proc)
        world.addAgent(agent1)
        world.addEntity(Entity("item1", "item"))
        world.addEntity(Entity("item2", "item"))
        world.load_declarations()
        world.initialize(False)


if __name__ == '__main__':
    unittest.main()
