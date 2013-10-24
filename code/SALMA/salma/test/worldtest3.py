import logging
import unittest
from scipy.odr.__odrpack import odr_stop

from salma import constants
from salma.SMCException import SMCException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions
from salma.model.core import Agent, Entity, Fluent, Action, \
    DeterministicAction, StochasticAction, \
    RandomActionOutcome, ExogenousAction, Constant
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
            World.logicsEngine = EclipseCLPEngine("../../ecl-test/domaindesc.ecl",
                                                  "../../ecl-test/example_procedures.ecl")
        except SMCException as e:
            print(e)
            raise
        logger = logging.getLogger('agamemnon-smc')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)

    def setUp(self):
        World.createNewWorld()
        world = World.getInstance()


    def test_load_declaration(self):
        world = World.getInstance()
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  ActionExecution("move_right", [Entity.SELF]))
        proc = Procedure("main", [], w)

        agent1 = Agent("rob1", "robot", proc)
        world.addAgent(agent1)
        world.addEntity(Entity("item1","item"))
        world.addEntity(Entity("item2","item"))
        world.load_declarations()
        world.initialize(False)
        fl, const = world.checkFluentInitialization()
        print(fl)
        print(const)

        a, a2, a3 = world.checkActionInitialization()
        print("---------")
        print(a)
        print(a2)
        print(a3)
        print("----")
        print(world.getAllActions())
if __name__ == '__main__':
    unittest.main()
