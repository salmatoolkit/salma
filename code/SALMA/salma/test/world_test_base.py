import logging
import unittest
import itertools

from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions, process
from salma.model.core import Entity, Fluent, Action, Constant
from salma.model.actions import DeterministicAction, StochasticAction, \
    RandomActionOutcome, Uniform, OutcomeSelectionStrategy
from salma.model.events import ExogenousAction
from salma.model.agent import Agent
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    Act, Procedure, While, Assign, ArbitraryAction, Variable, \
    Iterate, Select, ProcedureRegistry, ProcedureCall, If, Plan
from salma.model.world import World
from salma.test.testhelpers import withHeader


class BaseWorldTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):

        try:
            World.set_logic_engine(EclipseCLPEngine("ecl-test/domaindesc.ecl",
                                                    "ecl-test/example_procedures.ecl"))
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('salmalab')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)
        logger2 = logging.getLogger('salma.model')
        logger2.setLevel(logging.DEBUG)
        logger2.addHandler(ch)

    def setUp(self):
        print("setup")
        World.create_new_world()
        world = World.instance()
        world.load_declarations()

    def create_right_moving_mobot(self, robotId):
        """
        Creates a simple robot agent that moves one step to the right and then one step down.
        :param int robotId: the robot's id
        :rtype: Agent
        """
        seq = Sequence()
        seq.addChild(Act("move_right", [Entity.SELF]))
        seq.addChild(Act("move_down", [Entity.SELF]))

        proc = process.OneShotProcess(Procedure("main", [], seq))
        agent = Agent(robotId, "robot", [proc])
        return agent

    def setNoOneCarriesAnything(self):
        world = World.instance()
        robots = world.getDomain('robot')
        items = world.getDomain('item')
        for r in robots:
            for i in items:
                world.setFluentValue('carrying', [r.id, i.id], False)

    def place_agents_in_column(self, x=10, startY=10, distance=20):
        world = World.instance()

        y = startY

        # : :type r: Agent
        for r in world.getDomain('robot'):
            world.setFluentValue("xpos", [r.getId()], x)
            world.setFluentValue("ypos", [r.getId()], y)
            world.setFluentValue("active", [r.getId()], True)
            y += distance
