import logging
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import process
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.distributions import NormalDistribution, ExponentialDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import Sequence, Assign, SetFluent, Act
from salma.model.world import World
import random

__author__ = 'Christian'

import unittest


class EventSchedulingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        try:
            World.set_logic_engine(
                EclipseCLPEngine("ecl-test/event_scheduling/domaindesc_event_scheduling.ecl"))
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
        world.add_additional_expression_context_global("random", random)
        world.load_declarations()

    def create_random_walk_robot(self, robotId):
        def change_direction(agent:Agent=None, xpos=None, ypos=None, vx=None, vy=None):
            """
            :type xpos: (str) -> int
            :type ypos: (str) -> int
            :type vx: (str) -> int
            :type vy: (str) -> int
            """
            if xpos(agent.id) > 490:
                nvx = -1
            elif xpos(agent.id) < 10:
                nvx = 1
            else:
                nvx = vx(agent.id)

            if ypos(agent.id) > 490:
                nvy = -1
            elif ypos(agent.id) < 10:
                nvy = 1
            else:
                nvy = vy(agent.id)
            return nvx, nvy

        seq = Sequence([
            Assign("nv", EvaluationContext.EXTENDED_PYTHON_FUNCTION, change_direction, []),
            SetFluent("vx", [Entity.SELF], EvaluationContext.PYTHON_EXPRESSION, "nv[0]", []),
            SetFluent("vy", [Entity.SELF], EvaluationContext.PYTHON_EXPRESSION, "nv[0]", [])
        ])

        proc = process.TriggeredProcess(seq, EvaluationContext.PYTHON_EXPRESSION,
                                        "occur('wall_alert', self)", [])
        agent = Agent(robotId, "robot", [proc])
        return agent

    def test_movement(self):
        world = World.instance()

        world.setConstantValue("world_width", [], 500)
        world.setConstantValue("world_height", [], 500)
        world.setConstantValue("safety_distance", [], 10)

        rob1 = self.create_random_walk_robot("rob1")
        world.addAgent(rob1)
        world.initialize(False)
        world.setFluentValue("xpos", [rob1.id], 250)
        world.setFluentValue("ypos", [rob1.id], 250)
        world.setFluentValue("vx", [rob1.id], 1)
        world.setFluentValue("vy", [rob1.id], 0)
        world.setFluentValue("active", [rob1.id], True)
        world.setFluentValue("wheels_wet", [rob1.id], False)

        problematic_fluents = world.check_fluent_initialization()
        problematic_actions = world.check_action_initialization()
        print("problematic fluents: {}\n "
              "problematic actions: {}".format(problematic_fluents, problematic_actions))

        lightning = world.get_exogenous_action("lightning_strike")
        lightning.config.occurrence_distribution = ExponentialDistribution("integer", 0.01)

        world.step(100, evaluate_properties=False)
        print(world.getTime())
        world.step(100, evaluate_properties=False)
        print(world.getTime())


if __name__ == '__main__':
    unittest.main()

