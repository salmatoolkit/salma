import logging
import unittest
import itertools

from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions, process
from salma.model.core import Entity, Fluent, Action, Constant
from salma.model.actions import DeterministicAction, StochasticAction, \
    RandomActionOutcome, ExogenousAction, Uniform, OutcomeSelectionStrategy
from salma.model.agent import Agent
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    Act, Procedure, While, Assign, ArbitraryAction, Variable, \
    Iterate, Select, ProcedureRegistry, ProcedureCall, If, Plan
from salma.model.world import World
from salma.test.testhelpers import withHeader


class PropertyEvaluatorTest(unittest.TestCase):
    #: :type: EclipseCLPEngine
    engine = None

    @classmethod
    def setUpClass(cls):

        try:
            PropertyEvaluatorTest.engine = EclipseCLPEngine("ecl-test/domaindesc.ecl",
                                                            "ecl-test/example_procedures.ecl")
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('salma')
        logger.setLevel(logging.DEBUG)
        ch = logging.StreamHandler()
        logger.addHandler(ch)
        PropertyEvaluatorTest.engine.load_declarations()

    def setUp(self):
        engine = PropertyEvaluatorTest.engine
        engine.reset()

        engine.defineDomain("robot", ["rob1", "rob2"])
        engine.defineDomain("item", ["item1", "item2"])

        engine.setFluentValue("xpos", ["rob1"], 10)
        engine.setFluentValue("ypos", ["rob1"], 10)
        engine.setFluentValue("xpos", ["rob2"], 10)
        engine.setFluentValue("ypos", ["rob2"], 20)
        engine.setFluentValue("time", [], 0)
        engine.setConstantValue("gravity", [], 9.81)


    def test_nested_functions(self):
        engine = PropertyEvaluatorTest.engine
        result = engine.evaluate_ad_hoc("sqrt(sqrt(xpos(rob1)) * time * sqrt(xpos(rob1)) + 1 + gravity) $> 0")
        self.assertEqual(constants.OK, result)
        result = engine.evaluate_ad_hoc("sqrt(xpos(rob1) + time) $> 0")
        self.assertEqual(constants.OK, result)

        result = engine.evaluate_ad_hoc("sqrt(time * time) $>= 0")
        self.assertEqual(constants.OK, result)
        result = engine.evaluate_ad_hoc("sqrt(time * gravity) $>= 0")
        self.assertEqual(constants.OK, result)

        result = engine.evaluate_ad_hoc("sqrt(gravity * gravity) $>= 0")
        self.assertEqual(constants.OK, result)

        print(result)

    def test_fluent_arity_0(self):
        engine = PropertyEvaluatorTest.engine
        result = engine.evaluate_ad_hoc("time $= 0")
        self.assertEqual(constants.OK, result)
        print(result)

    def test_constant(self):
        engine = PropertyEvaluatorTest.engine
        result = engine.evaluate_ad_hoc("gravity * 10 \= 98.1")
        self.assertEqual(constants.OK, result)

        result = engine.evaluate_ad_hoc("gravity * 10 $>= 98.1")
        self.assertEqual(constants.OK, result)
        print(result)




if __name__ == '__main__':
    unittest.main()
