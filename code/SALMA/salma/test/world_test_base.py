import logging
import unittest
import itertools

from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions, process
from salma.model.core import Entity, Fluent, Action, Constant
from salma.model.actions import DeterministicAction, StochasticAction, \
    RandomActionOutcome, NonDeterministic
from salma.model.selectionstrategy import OutcomeSelectionStrategy, NonDeterministic
from salma.model.events import ExogenousAction
from salma.model.agent import Agent
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution, ConstantDistribution, Never, Zero, \
    OptionalDistribution, ExponentialDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Statement, Sequence, \
    Act, Procedure, While, Assign, FunctionStatement, Variable, \
    Iterate, Select, ProcedureRegistry, ProcedureCall, If, Plan, Wait
from salma.model.world import World
from salma.test.testhelpers import withHeader


class BaseWorldTest(unittest.TestCase):

    def configure_events_default(self):
        world = World.instance()
        world.deactivate_info_transfer()
        finish_step = world.get_exogenous_action("finish_step")
        finish_step.config.occurrence_distribution = ConstantDistribution("integer", 5)
        accidental_drop = world.get_exogenous_action("accidental_drop")
        accidental_drop.config.occurrence_distribution = OptionalDistribution(0.0,
                                                                              ExponentialDistribution("integer", 0.1))
        collision = world.get_exogenous_action("collision")
        collision.config.occurrence_distribution = BernoulliDistribution(1.0)
        collision.config.set_param_distribution("severity", Zero())

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
        World.create_new_world(erase_properties=True)
        world = World.instance()
        world.load_declarations()
        world.set_constant_value("gravity", [], 9.81)
        world.register_clp_function("robotLeftFrom")
        self.configure_events_default()

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
                world.set_fluent_value('carrying', [r.id, i.id], False)

    def initialize_robot(self, robot_id, x, y, vx, vy, radius=1, active=True, partner=None):
        world = World.instance()
        world.set_fluent_value("xpos", [robot_id], x)
        world.set_fluent_value("ypos", [robot_id], y)
        world.set_fluent_value("vx", [robot_id], vx)
        world.set_fluent_value("vy", [robot_id], vy)
        world.set_constant_value("robot_radius", [robot_id], radius)
        world.set_fluent_value("active", [robot_id], active)
        world.set_fluent_value("partner", [robot_id], partner)
        items = world.getDomain('item')
        for i in items:
            world.set_fluent_value('carrying', [robot_id, i.id], False)

    def initialize_items(self):
        world = World.instance()
        items = world.getDomain('item')
        for i in items:
            world.set_fluent_value("painted", [i.id], False)
            world.set_fluent_value("marking", [i.id], None)

    def place_agents_in_column(self, x=10, startY=10, distance=20):
        world = World.instance()

        y = startY

        # : :type r: Agent
        for r in world.getDomain('robot'):
            self.initialize_robot(r.id, x, y, 0, 0)
            y += distance
