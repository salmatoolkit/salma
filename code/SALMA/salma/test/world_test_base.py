import logging
import unittest
import itertools

from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions, process
from salma.model.core import Entity, Fluent, Action, Constant
from salma.model.actions import DeterministicAction, StochasticAction, \
    RandomActionOutcome, Uniform
from salma.model.selectionstrategy import OutcomeSelectionStrategy, Uniform
from salma.model.events import ExogenousAction
from salma.model.agent import Agent
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution, ConstantDistribution, Never, Zero
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    Act, Procedure, While, Assign, FunctionControlNode, Variable, \
    Iterate, Select, ProcedureRegistry, ProcedureCall, If, Plan, Wait
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
        world.setConstantValue("gravity", [], 9.81)
        world.register_clp_function("robotLeftFrom")
        world.deactivate_info_transfer()
        ea = world.get_exogenous_action("collision")
        ea.config.occurrence_distribution = Never()
        ea.config.set_param_distribution("severity", Zero())

    def create_right_moving_mobot(self, robotId):
        """
        Creates a simple robot agent that moves one step to the right and then one step down.
        :param int robotId: the robot's id
        :rtype: Agent
        """
        seq = Sequence()
        seq.add_child(Act("move_right", [Entity.SELF]))
        seq.add_child(Wait("occur('finish_step', self)"))
        seq.add_child(Act("move_down", [Entity.SELF]))
        seq.add_child(Wait("occur('finish_step', self)"))
        proc = process.OneShotProcess(Procedure(seq))
        agent = Agent(robotId, "robot", [proc])
        return agent

    def setNoOneCarriesAnything(self):
        world = World.instance()
        robots = world.getDomain('robot')
        items = world.getDomain('item')
        for r in robots:
            for i in items:
                world.setFluentValue('carrying', [r.id, i.id], False)

    def initialize_robot(self, robot_id, x, y, vx, vy):
        world = World.instance()
        world.setFluentValue("xpos", [robot_id], x)
        world.setFluentValue("ypos", [robot_id], y)
        world.setFluentValue("vx", [robot_id], vx)
        world.setFluentValue("vy", [robot_id], vy)
        world.setConstantValue("robot_radius", [robot_id], 1)
        world.setFluentValue("active", [robot_id], True)
        world.setFluentValue("partner", [robot_id], None)
        items = world.getDomain('item')
        for i in items:
            world.setFluentValue('carrying', [robot_id, i.id], False)

    def initialize_items(self):
        world = World.instance()
        items = world.getDomain('item')
        for i in items:
            world.setFluentValue("painted", [i.id], False)
            world.setFluentValue("marking", [i.id], None)

    def place_agents_in_column(self, x=10, startY=10, distance=20):
        world = World.instance()

        y = startY

        # : :type r: Agent
        for r in world.getDomain('robot'):
            world.setFluentValue("xpos", [r.getId()], x)
            world.setFluentValue("ypos", [r.getId()], y)
            world.setFluentValue("active", [r.getId()], True)
            y += distance
