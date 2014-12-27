import math
from salma import constants
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.model.experiment import Experiment
from salma.model.selectionstrategy import OutcomeSelectionStrategy, Uniform
from salma.model.distributions import BernoulliDistribution, ConstantDistribution, OptionalDistribution, \
    ExponentialDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, While, Act, Wait, Procedure, Sequence, Assign, ArbitraryAction, Variable
from salma.model.world import World
from salma.test.world_test_base import BaseWorldTest


def print_value(value):
    print("Val: ", value)
    return ControlNode.CONTINUE, None


class MySelectionStrategy(OutcomeSelectionStrategy):
    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluation_context, param_values):
        """
        :type evaluation_context: EvaluationContext
        :type param_values: list
        """
        x = evaluation_context.getFluentValue('xpos', param_values[0])
        height = param_values[1]
        if x > 100 or height > 50:
            return self.options["crash"]
        else:
            return self.options["land_on"]


class WorldTest(BaseWorldTest):
    def __configure_events_default(self):
        world = World.instance()
        finish_step = world.get_exogenous_action("finish_step")
        finish_step.config.occurrence_distribution = ConstantDistribution("integer", 5)
        accidental_drop = world.get_exogenous_action("accidental_drop")
        accidental_drop.config.occurrence_distribution = OptionalDistribution(0.0,
                                                                              ExponentialDistribution("integer", 0.1))
        collision = world.get_exogenous_action("collision")
        collision.config.occurrence_distribution = BernoulliDistribution(1.0)

    def test_world_step_explicit(self):
        world = World.instance()
        world.addAgent(self.create_right_moving_mobot('rob1'))
        world.initialize(False)
        self.__configure_events_default()
        self.initialize_robot("rob1", 10, 10, 0, 0)

        print("\n\n----\n\n")
        print("INIT:")
        print("\n\n----\n\n")
        world.printState()

        verdict, finished, toplevel_results, *details = world.step(100)

        print("\n\n----\n\n")
        print("AFTER STEP 1:")
        print("\n\n----\n\n")
        world.printState()
        self.assertEqual(world.getTime(), 5)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 10)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 10)
        self.assertEqual(world.getFluentValue("vx", ["rob1"]), 1)
        self.assertEqual(world.getFluentValue("vy", ["rob1"]), 0)

        world.step(100)

        print("\n\n----\n\n")
        print("AFTER STEP 2:")
        print("\n\n----\n\n")
        world.printState()

        self.assertEqual(world.getTime(), 10)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 10)
        self.assertEqual(world.getFluentValue("vx", ["rob1"]), 0)
        self.assertEqual(world.getFluentValue("vy", ["rob1"]), 1)
        world.step(100)
        print("\n\n----\n\n")
        print("AFTER STEP 3:")
        print("\n\n----\n\n")
        world.printState()
        self.assertEqual(world.getTime(), 10)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("vx", ["rob1"]), 0)
        self.assertEqual(world.getFluentValue("vy", ["rob1"]), 0)

    def test_world_run_until_end(self):
        world = World.instance()
        world.addAgent(self.create_right_moving_mobot('rob1'))
        world.initialize(False)
        self.__configure_events_default()
        self.initialize_robot("rob1", 10, 10, 0, 0)

        print("\n\n----\n\n")
        print("INIT:")
        print("\n\n----\n\n")
        world.printState()
        experiment = Experiment(world)
        verdict, results = experiment.run_until_finished()
        print("\n\n----\n\n")
        print("END:")
        print("\n\n----\n\n")
        world.printState()
        print(results)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("vx", ["rob1"]), 0)
        self.assertEqual(world.getFluentValue("vy", ["rob1"]), 0)
        self.assertTrue(world.is_finished())
        self.assertEqual(world.getTime(), 10)

    def test_run_right_until_max_X_pos(self):
        world = World.instance()

        w = While("robotLeftFrom",
                  [Entity.SELF, 100, "s0"],
                  [
                      Act("move_right", [Entity.SELF]),
                      Wait("not moving(self)")
                  ])

        agent = Agent("rob1", "robot", Procedure(w))
        world.addAgent(agent)
        world.initialize(False)
        self.__configure_events_default()
        self.initialize_robot("rob1", 10, 10, 0, 0)

        world.printState()
        experiment = Experiment(world)
        verdict, info = experiment.run_until_finished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))

        print("----")
        world.printState()
        print("----\n\n")

        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 100)
        self.assertEqual(verdict, constants.OK)
        self.assertTrue(450 <= world.getTime() <= 451)
        self.assertTrue(world.is_finished())
        print(info)

    def test_variable_assignment(self):
        world = World.instance()

        # run from (x,y) to (y,y)
        seq = Sequence([
            Assign("myY", "ypos", [Entity.SELF]),
            ArbitraryAction(print_value, [Variable("myY")])
        ])
        w = While("robotLeftFrom",
                  [Entity.SELF, Variable("myY"), "s0"],
                  [
                      Act("move_right", [Entity.SELF]),
                      Wait("not moving(self)"),
                      Assign("myX", "xpos", [Entity.SELF]),
                      ArbitraryAction(print_value, [Variable("myX")])
                  ])
        seq.add_child(w)

        agent = Agent("rob1", "robot", Procedure(seq))
        world.addAgent(agent)

        world.initialize(False)

        self.initialize_robot("rob1", 10, 20, 0, 0)
        self.__configure_events_default()
        experiment = Experiment(world)
        experiment.run_until_finished()
        self.assertEqual(agent.evaluation_context.resolve(Variable('myY'))[0], 20)
        self.assertEqual(agent.evaluation_context.resolve(Variable('myX'))[0], 20)

    def test_evaluate_python_expression(self):
        world = World.instance()
        world.add_additional_expression_context_global("math", math)
        # run from (x,y) to (y,y)
        seq = Sequence([
            Assign("x", "6"),
            Assign("y", "x * 7 + params[0]", [3]),
            Assign("z", "0", []),
            Assign("z2", "xpos('rob1') + ypos('rob1')"),
            # test symbols without quotation marks
            Assign("z3", "xpos(rob1) + ypos(rob1)"),
            Assign("z4", "math.sqrt(xpos(rob1) - 1)"),
            Assign("z5", "gravity() * 10"),
            Assign("z6", "dist_from_origin(rob1)"),
            While("z < y - params[0]", [3],
                  [
                      Assign("z", "z + 1")])
        ])

        agent = Agent("rob1", "robot", Procedure(seq))
        world.addAgent(agent)

        world.initialize(False)
        self.__configure_events_default()
        self.initialize_robot("rob1", 10, 15, 0, 0)

        world.printState()

        experiment = Experiment(world)
        experiment.run_until_finished()

        self.assertEqual(agent.evaluation_context.resolve(Variable("x"))[0], 6)
        self.assertEqual(agent.evaluation_context.resolve(Variable("y"))[0], 45)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z"))[0], 42)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z2"))[0], 25)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z3"))[0], 25)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z4"))[0], 3)

        v = agent.evaluation_context.resolve(Variable("z5"))[0]
        self.assertAlmostEqual(98.1, v)
        v2 = agent.evaluation_context.resolve(Variable("z6"))[0]
        self.assertAlmostEqual(math.sqrt(10 * 10 + 15 * 15), v2)

    def test_evaluate_python_function(self):
        world = World.instance()

        def myfunc1(a, b):
            return a * b

        def myfunc2(a, b, **ctx):
            return a * b * ctx["x"] * ctx["xpos"]("rob1")

        def myfunc3(a, b, x=None, xpos=None, ypos=None, **ctx):
            return a * b * x * xpos("rob1") * ypos("rob1")

        seq = Sequence([
            Assign("x", lambda i: i ** 2, [3]),
            Assign("y", myfunc1, [4, 6]),
            Assign("z", myfunc2, [-1, 2]),
            Assign("z2", myfunc3, [-1, 2])
        ])

        agent = Agent("rob1", "robot", Procedure(seq))
        world.addAgent(agent)

        world.initialize(False)
        self.__configure_events_default()
        self.initialize_robot("rob1", 10, 15, 0, 0)

        world.printState()

        experiment = Experiment(world)
        experiment.run_until_finished()

        self.assertEqual(agent.evaluation_context.resolve(Variable("x"))[0], 9)
        self.assertEqual(agent.evaluation_context.resolve(Variable("y"))[0], 24)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z"))[0], -1 * 2 * 9 * 10)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z2"))[0], -1 * 2 * 9 * 10 * 15)


    def record_outcomes(self, world, actions=None, **ctx):
        for a in actions:
            if a[0] == "crash":
                self.__crash_count += 1
            elif a[1] == "land_on":
                self.land_on_count += 1

        print("My Actions: {}".format(actions))
        return True, None

    def testUniformStochasticAction(self):
        world = World.instance()

        seq = Sequence([
            Act("move_right", [Entity.SELF]),
            Act("jump", [Entity.SELF, 42]),
            Act("jump", [Entity.SELF, 42]),
            Act("jump", [Entity.SELF, 42])
        ])

        agent = Agent("rob1", "robot", Procedure(seq))
        world.addAgent(agent)
        world.initialize(False)

        self.__configure_events_default()
        jump_action = world.get_stochastic_action("jump")
        self.generate_outcomes(jump_action)
        jump_action.selection_strategy = Uniform()

        self.initialize_robot("rob1", 10, 20, 0, 0)

        world.printState()
        self.__crash_count = 0
        self.__land_on_count = 0
        experiment = Experiment(world)
        experiment.run_until_finished(step_listeners=[self.record_outcomes])
        world.printState()
        print(self.__crash_count)

    def generate_outcomes(self, jump_action):
        """
        :type jump_action: StochasticAction
        """
        land_on = jump_action.outcome("land_on")
        land_on.map_param("r", "r")
        land_on.uniform_param("x", (100, 500))
        land_on.uniform_param("y", (0, 200))

        crash = jump_action.outcome("crash")
        crash.map_param("r", "r")

