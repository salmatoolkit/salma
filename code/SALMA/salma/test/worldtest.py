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
    ActionExecution, Procedure, While, VariableAssignment, ArbitraryAction, Variable, \
    Iterate, SelectFirst, ProcedureRegistry, ProcedureCall, If, Plan
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest


def printValue(value):
    print("Val: ", value)
    return (ControlNode.CONTINUE, None)


class MySelectionStrategy(OutcomeSelectionStrategy):

    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluationContext, paramValues):
        """
        :type evaluationContext: EvaluationContext
        :type paramValues: list
        """
        x = evaluationContext.getFluentValue('xpos', paramValues[0])
        height = paramValues[1]
        if x > 100 or height > 50:
            return self.action.outcome("crash")
        else:
            return self.action.outcome("land_on")


class WorldTest(BaseWorldTest):

    @withHeader
    def testWorldStepExplicit(self):
        world = World.instance()
        world.addAgent(self.create_right_moving_mobot('rob1'))
        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 10)

        self.setNoOneCarriesAnything()

        print("\n\n----\n\n")
        print("INIT:")
        print("\n\n----\n\n")
        world.printState()

        finished, overallVerdict, toplevel_results, scheduled_results, actions, failedRegularActions = world.step()

        self.assertFalse(finished)
        self.assertDictEqual(toplevel_results, {})
        self.assertEqual(11, world.getFluentValue('xpos', ['rob1']))
        self.assertEqual(10, world.getFluentValue('ypos', ['rob1']))
        self.assertEqual(1, world.getTime())

        print("\n\n----\n\n")
        print("AFTER STEP 1:")
        print("\n\n----\n\n")
        world.printState()

        world.step()
        self.assertEqual(11, world.getFluentValue('xpos', ['rob1']))
        self.assertEqual(11, world.getFluentValue('ypos', ['rob1']))
        self.assertEqual(2, world.getTime())

        print("\n\n----\n\n")
        print("AFTER STEP 2:")
        print("\n\n----\n\n")
        world.printState()

        world.step()
        print("\n\n----\n\n")
        print("AFTER STEP 3:")
        print("\n\n----\n\n")
        world.printState()

    @withHeader
    def testWorldRunUntilEnd(self):
        world = World.instance()
        world.addAgent(self.create_right_moving_mobot('rob1'))
        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)

        self.setNoOneCarriesAnything()
        print("INIT:")
        print("----")
        world.printState()
        print("----\n\n")

        verdict, info = world.runUntilFinished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))
        print("----")
        world.printState()
        print("----\n\n")
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 11)
        self.assertEqual(world.getFluentValue("ypos", ["rob1"]), 16)
        self.assertEqual(verdict, constants.OK)
        self.assertEqual(info['steps'], 4)


    @withHeader
    def testRunRightUntilMaxXPos(self):
        world = World.instance()

        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom",
                  [Entity.SELF, 18],
                  ActionExecution("move_right", [Entity.SELF]))

        agent = Agent("rob1", "robot", Procedure("main", [], w))
        world.addAgent(agent)
        world.initialize(False)

        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 10)
        self.setNoOneCarriesAnything()

        world.printState()
        verdict, info = world.runUntilFinished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))

        print("----")
        world.printState()
        print("----\n\n")

        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 18)
        self.assertEqual(verdict, constants.OK)
        self.assertEqual(info['steps'], 10)


    @withHeader
    def testTwoAgentsRunUntilMaxXPos(self):
        world = World.instance()

        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom", [Entity.SELF, 120],
                  ActionExecution("move_right", [Entity.SELF]))

        proc = Procedure("main", [], w)

        agent1 = Agent("rob1", "robot", proc)

        agent2 = Agent("rob2", "robot", proc)

        world.addAgent(agent1)
        world.addAgent(agent2)
        world.initialize(False)

        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 10)

        world.setFluentValue("xpos", ["rob2"], 20)
        world.setFluentValue("ypos", ["rob2"], 20)

        self.setNoOneCarriesAnything()
        verdict, info = world.runUntilFinished()
        print("Verdict: {}  after {} steps".format(verdict, info['steps']))
        print("----")
        world.printState()
        print("----\n\n")

        self.assertEqual(verdict, constants.OK)
        # there should be 110 steps to take rob1 to 120 and 2 extra steps
        self.assertEqual(info['steps'], 112)
        self.assertEqual(world.getFluentValue("xpos", ["rob1"]), 120)
        self.assertEqual(world.getFluentValue("xpos", ["rob2"]), 120)

    @withHeader
    def testVariableAssignment(self):
        world = World.instance()

        # run from (x,y) to (y,y)
        seq = Sequence([
            VariableAssignment("myY", EvaluationContext.FLUENT, "ypos", [Entity.SELF]),
            ArbitraryAction(printValue, [Variable("myY")])
        ])
        w = While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom",
                  [Entity.SELF, Variable("myY")],
                  Sequence([
                      ActionExecution("move_right", [Entity.SELF]),
                      VariableAssignment("myX", EvaluationContext.FLUENT, "xpos", [Entity.SELF]),
                      ArbitraryAction(printValue, [Variable("myX")])
                  ])
        )
        seq.addChild(w)

        agent = Agent("rob1", "robot", Procedure("main", [], seq))
        world.addAgent(agent)

        world.initialize(False)

        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 20)
        self.setNoOneCarriesAnything()

        world.runUntilFinished()
        self.assertEqual(agent.evaluation_context.resolve(Variable('myY'))[0], 20)
        self.assertEqual(agent.evaluation_context.resolve(Variable('myX'))[0], 20)

    @withHeader
    def test_evaluate_python_expression(self):
        world = World.instance()

        # run from (x,y) to (y,y)
        seq = Sequence([
            VariableAssignment("x",
                               EvaluationContext.PYTHON_EXPRESSION,
                               "6",
                []),
            VariableAssignment("y",
                               EvaluationContext.PYTHON_EXPRESSION,
                               "x * 7 + params[0]",
                               [3]),
            VariableAssignment("z",
                               EvaluationContext.PYTHON_EXPRESSION,
                               "0",
                []),
            VariableAssignment("z2",
                               EvaluationContext.PYTHON_EXPRESSION,
                               "xpos('rob1') + ypos('rob1')",
                []),

            While(EvaluationContext.PYTHON_EXPRESSION,
                  "z < y - params[0]",
                  [3],
                  Sequence([
                      VariableAssignment("z",
                                         EvaluationContext.PYTHON_EXPRESSION,
                                         "z + 1",
                          [])
                  ])
            )
        ])

        agent = Agent("rob1", "robot", Procedure("main", [], seq))
        world.addAgent(agent)

        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)

        world.runUntilFinished()

        self.assertEqual(agent.evaluation_context.resolve(Variable("x"))[0], 6)
        self.assertEqual(agent.evaluation_context.resolve(Variable("y"))[0], 45)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z"))[0], 42)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z2"))[0], 25)

    @withHeader
    def test_evaluate_python_function(self):
        world = World.instance()

        def myfunc1(x,y):
            return x*y

        def myfunc2(a, b, **ctx):
            return a*b*ctx["x"]*ctx["xpos"]("rob1")

        def myfunc3(a, b, x=None, xpos=None, ypos=None, **ctx):
            return a * b * x * xpos("rob1") * ypos("rob1")

        seq = Sequence([
            VariableAssignment("x", EvaluationContext.PYTHON_FUNCTION,
                               lambda i : i**2, [3]),
            VariableAssignment("y", EvaluationContext.PYTHON_FUNCTION,
                               myfunc1, [4,6]),
            VariableAssignment("z", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                               myfunc2, [-1, 2]),
            VariableAssignment("z2", EvaluationContext.EXTENDED_PYTHON_FUNCTION,
                               myfunc3, [-1, 2])
            ])

        agent = Agent("rob1", "robot", Procedure("main", [], seq))
        world.addAgent(agent)

        world.initialize(False)
        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 15)

        world.runUntilFinished()

        self.assertEqual(agent.evaluation_context.resolve(Variable("x"))[0], 9)
        self.assertEqual(agent.evaluation_context.resolve(Variable("y"))[0], 24)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z"))[0], -1*2*9*10)
        self.assertEqual(agent.evaluation_context.resolve(Variable("z2"))[0], -1*2*9*10*15)

    @withHeader
    def testRandomizeFluents(self):
        world = World.instance()
        world.getFluent("xpos")

        world.addAgent(self.create_right_moving_mobot('rob3'))
        world.addAgent(self.create_right_moving_mobot('rob4'))

        for i in range(1, 10):
            print("Try {}".format(i))
            print("-" * 50)
            world.reset()
            world.sample_fluent_values()
            world.printState()

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


    @withHeader
    def testUniformStochasticAction(self):
        world = World.instance()

        jump_action = world.get_stochastic_action("jump")
        self.generate_outcomes(jump_action)
        jump_action.selection_strategy = Uniform()

        seq = Sequence([
            ActionExecution("move_right", [Entity.SELF]),
            ActionExecution("jump", [Entity.SELF, 42]),
            ActionExecution("jump", [Entity.SELF, 42]),
            ActionExecution("jump", [Entity.SELF, 42])
        ])

        agent = Agent("rob1", "robot", Procedure("main", [], seq))
        world.addAgent(agent)
        world.initialize(False)

        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 20)
        world.setFluentValue("active", ["rob1"], True)

        world.runUntilFinished()
        world.printState()

    @withHeader
    def testCustomStochasticAction(self):
        world = World.instance()
        jump_action = world.get_stochastic_action("jump")
        self.generate_outcomes(jump_action)

        jump_action.selection_strategy = MySelectionStrategy()

        seq1 = Sequence([
            ActionExecution("move_right", [Entity.SELF]),
            ActionExecution("jump", [Entity.SELF, 42]),
            ActionExecution("jump", [Entity.SELF, 42]),
            ActionExecution("jump", [Entity.SELF, 42])
        ])

        seq2 = Sequence([
            ActionExecution("move_right", [Entity.SELF]),
            ActionExecution("jump", [Entity.SELF, 55]),
            ActionExecution("jump", [Entity.SELF, 55]),
            ActionExecution("jump", [Entity.SELF, 55])
        ])

        agent1 = Agent("rob1", "robot", Procedure("main", [], seq1))
        agent2 = Agent("rob2", "robot", Procedure("main", [], seq1))
        agent3 = Agent("rob3", "robot", Procedure("main", [], seq2))

        world.addAgent(agent1)
        world.addAgent(agent2)
        world.addAgent(agent3)
        world.initialize(False)

        world.setFluentValue("xpos", ["rob1"], 10)
        world.setFluentValue("ypos", ["rob1"], 20)
        world.setFluentValue("active", ["rob1"], True)

        world.setFluentValue("xpos", ["rob2"], 110)
        world.setFluentValue("ypos", ["rob2"], 20)
        world.setFluentValue("active", ["rob2"], True)

        world.setFluentValue("xpos", ["rob3"], 10)
        world.setFluentValue("ypos", ["rob3"], 50)
        world.setFluentValue("active", ["rob3"], True)

        world.runUntilFinished()
        world.printState()

    @withHeader
    def testExogenousAction(self):

        world = World.instance()  # : :type world: World
        drop = world.get_exogenous_action("accidental_drop")
        drop.config.occurrence_distribution = BernoulliDistribution(0.7)

        collision_event = world.get_exogenous_action("collision")

        collision_event.config.occurrence_distribution = BernoulliDistribution(0.7)
        #collision_event.config.uniform_param("integer", value_range=(0, 100))
        collision_event.config.uniform_param("severity", value_range=(0, 100))

        seq = Sequence([
            ActionExecution("move_right", [Entity.SELF]),
            ActionExecution("move_right", [Entity.SELF]),
            ActionExecution("move_right", [Entity.SELF]),
            ActionExecution("move_right", [Entity.SELF]),
            ActionExecution("move_right", [Entity.SELF]),
        ])

        agent1 = Agent("rob1", "robot", Procedure("main", [], seq))
        agent2 = Agent("rob2", "robot", Procedure("main", [], seq))
        agent3 = Agent("rob3", "robot", Procedure("main", [], seq))

        world.addAgent(agent1)
        world.addAgent(agent2)
        world.addAgent(agent3)
        world.initialize(False)

        self.place_agents_in_column()
        world.setFluentValue("carrying", ["rob1", "coffee"], True)
        world.setFluentValue("xpos", ["rob1"], world.getFluentValue("xpos", ["rob2"]))
        world.setFluentValue("ypos", ["rob1"], world.getFluentValue("ypos", ["rob2"]))

        print('BEFORE:')
        world.printState()

        world.runUntilFinished()

        print('\n' * 5)
        print('AFTER:')
        world.printState()

    @withHeader
    def testGrabTwiceBySameAgentImpossible(self):
        world = World.instance()
        seq1 = Sequence([
            ActionExecution("grab", [Entity.SELF, "coffee"]),
            ActionExecution("grab", [Entity.SELF, "coffee"])
        ])
        seq2 = Sequence([
            ActionExecution("grab", [Entity.SELF, "chocolate"]),
            ActionExecution("move_right", [Entity.SELF])
        ])

        agent1 = Agent("rob1", "robot", Procedure("main", [], seq1))
        agent2 = Agent("rob2", "robot", Procedure("main", [], seq2))

        world.addAgent(agent1)
        world.addAgent(agent2)

        world.initialize(False)

        self.place_agents_in_column()

        self.setNoOneCarriesAnything()

        print('BEFORE:')
        world.printState()

        finished, overallVerdict, toplevel_results, scheduled_results, actions, failedRegularActions = world.step()
        print("Executed: {}".format(actions))
        print('AFTER STEP 1:')
        world.printState()

        self.assertFalse(finished)
        self.assertEqual(overallVerdict, constants.OK)
        self.assertListEqual(failedRegularActions, [])

        finished, overallVerdict, toplevel_results, scheduled_results, actions, failedRegularActions = world.step()
        print("\n\nExecuted: {}".format(actions))
        print('AFTER STEP 1:')
        world.printState()

        self.assertFalse(finished)
        self.assertEqual(overallVerdict, constants.NOT_OK)
        self.assertListEqual(failedRegularActions, [('grab', ['rob1', 'coffee'])])

    def setupSelectionContext(self):
        world = World.instance()

        item1 = Entity("item1", "item")
        item2 = Entity("item2", "item")

        world.addEntity(item1)
        world.addEntity(item2)

        seq1 = Sequence([
            ActionExecution("grab", [Entity.SELF, "item1"])
        ])
        seq2 = Sequence([
            ActionExecution("grab", [Entity.SELF, "item2"])
        ])

        #implicitly creates one-shot processes
        agent1 = Agent("rob1", "robot", Procedure("main", [], seq1))
        agent2 = Agent("rob2", "robot", Procedure("main", [], seq2))

        grabMap = {"rob1": item1, "rob2": item2}
        world.addAgent(agent1)
        world.addAgent(agent2)

        world.initialize(False)

        self.place_agents_in_column()

        self.setNoOneCarriesAnything()

        return agent1, agent2, grabMap

    @withHeader
    def testSelectAll(self):
        world = World.instance()
        agent1, agent2, grabMap = self.setupSelectionContext()
        res1 = agent1.evaluation_context.selectAll(EvaluationContext.FLUENT, "carrying",
                                                   ('r', 'robot'), ('i', 'item'))
        self.assertListEqual(res1, [])
        world.runUntilFinished()

        res2 = agent1.evaluation_context.selectAll(EvaluationContext.FLUENT, "carrying",
                                                   ('r', 'robot'), ('i', 'item'))

        self.assertEqual(len(res2), 2)

        handledAgents = set()
        for entry in res2:
            self.assertIsInstance(
                entry['r'], Agent)
            if entry['r'].id == "rob1":
                self.assertEqual(entry['r'], agent1)
            elif entry['r'].id == "rob2":
                self.assertEqual(entry['r'], agent2)
            else:
                self.fail("Wrong agent id.")

            self.assertEqual(entry['i'], grabMap[entry['r'].id])
            handledAgents.add(entry['r'].id)

        self.assertSetEqual(handledAgents, set(['rob1', 'rob2']))

        print(res2)

    @withHeader
    def testSelectFirst(self):
        world = World.instance()
        agent1, agent2, grabMap = self.setupSelectionContext()
        res1 = agent1.evaluation_context.selectFirst(EvaluationContext.FLUENT, "carrying",
                                                     ('r', 'robot'), ('i', 'item'))
        self.assertIsNone(res1)
        world.runUntilFinished()

        res2 = agent1.evaluation_context.selectFirst(EvaluationContext.FLUENT, "carrying",
                                                     ('r', 'robot'), ('i', 'item'))

        self.assertIsInstance(res2, dict)

        handledAgents = set()

        self.assertIsInstance(
            res2['r'], Agent)
        if res2['r'].id == "rob1":
            self.assertEqual(res2['r'], agent1)
        elif res2['r'].id == "rob2":
            self.assertEqual(res2['r'], agent2)
        else:
            self.fail("Wrong agent id.")

        self.assertEqual(res2['i'], grabMap[res2['r'].id])
        handledAgents.add(res2['r'].id)

        print(res2)

    @withHeader
    def testIterate(self):
        world = World.instance()
        items = []
        for i in range(5):
            item = Entity("item{}".format(i), "item")
            world.addEntity(item)
            items.append(item)

        seq1 = Sequence([
            Iterate(EvaluationContext.TRANSIENT_FLUENT, "canPaint",
                    [Entity.SELF, ("i", "item")],
                    Sequence([
                        ActionExecution("paint", [Entity.SELF, Variable("i")])
                    ])

            )
        ])

        agent = Agent("rob1", "robot", Procedure("main", [], seq1))
        world.addAgent(agent)

        world.initialize(False)
        self.setNoOneCarriesAnything()
        for item in items:
            world.setFluentValue("painted", [item.id], False)

        world.runUntilFinished()
        world.printState()
        for item in items:
            self.assertTrue(world.getFluentValue("painted", [item.id]))

    @withHeader
    def testSelectFirstWorld(self):
        world = World.instance()

        for i in range(5):
            item = Entity("item{}".format(i), "item")
            world.addEntity(item)

        seq1 = Sequence([
            SelectFirst(EvaluationContext.TRANSIENT_FLUENT, "canPaint",
                        [Entity.SELF, ("i", "item")]),
            ActionExecution("paint", [Entity.SELF, Variable("i")])
        ])
        agent = Agent("rob1", "robot", Procedure("main", [], seq1))
        world.addAgent(agent)

        world.initialize(False)
        self.setNoOneCarriesAnything()
        items = world.getDomain('item')
        for item in items:
            world.setFluentValue("painted", [item.id], False)

        world.runUntilFinished()
        world.printState()
        paintedItems = []
        for item in items:
            if world.getFluentValue("painted", [item.id]):
                paintedItems.append(item)

        self.assertEqual(len(paintedItems), 1)
        self.assertEqual(paintedItems[0],
                         agent.evaluation_context.getEntity(
                             agent.evaluation_context.resolve(Variable("i", "item"))[0]
                         )
        )


    @withHeader
    def test_procedure_call(self):

        world = World.instance()
        transportToX = Procedure("transportToX",
                                 [("r1", "robot"), ("i", "item"), ("targetX", "integer")],
                                 Sequence(
                                     [
                                         ActionExecution("grab", [Variable("r1"), Variable("i")]),
                                         While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom",
                                               [Variable("r1"), Variable("targetX")],
                                               ActionExecution("move_right", [Variable("r1")])),
                                         ActionExecution("drop", [Variable("r1"), Variable("i")])
                                     ]
                                 ))
        registry = ProcedureRegistry()
        registry.registerProcedure(transportToX)

        controlProc = Procedure("main", [],
                                Sequence([
                                    ProcedureCall("transportToX", ["rob1", "coffee", 17])
                                ]))

        agent = Agent("rob1", "robot", controlProc, registry)
        world.addAgent(agent)

        world.initialize(False)

        self.place_agents_in_column(10)

        world.runUntilFinished()
        world.printState()
        self.assertEqual(world.getFluentValue('xpos', ['rob1']), 17)

    @withHeader
    def test_recursive_procedure_call(self):
        world = World.instance()
        transportToX = Procedure("transportToX",
                                 [("r1", "robot"), ("i", "item"), ("targetX", "integer")],
                                 Sequence(
                                     [
                                         ActionExecution("grab", [Variable("r1"), Variable("i")]),
                                         While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom",
                                               [Variable("r1"), Variable("targetX")],
                                               ActionExecution("move_right", [Variable("r1")])
                                         ),
                                         ActionExecution("drop", [Variable("r1"), Variable("i")]),
                                         # test recursion
                                         If(EvaluationContext.PYTHON_FUNCTION,
                                            lambda i: i == "coffee",
                                            [Variable("i")],
                                            ProcedureCall("transportToX", ["rob1", "chocolate", 25]),
                                            None)

                                     ]
                                 )
        )
        registry = ProcedureRegistry()
        registry.registerProcedure(transportToX)

        controlProc = Procedure("main", [],
                                Sequence([
                                    ProcedureCall("transportToX", ["rob1", "coffee", 17])
                                ]))

        agent = Agent("rob1", "robot", controlProc, registry)
        world.addAgent(agent)

        world.initialize(False)

        self.place_agents_in_column(10)

        world.runUntilFinished()
        world.printState()
        self.assertEqual(world.getFluentValue('xpos', ['rob1']), 25)

    @withHeader
    def testCreatePLan_OK_Unique(self):
        world = World.instance()
        controlProc = Procedure("main", [],
                                Sequence([
                                    Plan("transportToX",
                                         [("r", "robot"),
                                          ("i", "item"),
                                          17],
                                         "plan1")
                                ]))

        agent = Agent("rob1", "robot", controlProc)
        world.addAgent(agent)

        world.initialize(False)

        self.place_agents_in_column(10)

        world.runUntilFinished()
        world.printState()

        resolvedValues = agent.evaluation_context.resolve(
            Variable("r"),
            Variable("i"),
            Variable("plan"))
        print("resolvedValues:", resolvedValues)

    @withHeader
    def testEnumerateFluentInstances(self):
        world = World.instance()
        rob1 = Agent("rob1", "robot", Procedure("main", [], Sequence([])))
        world.addAgent(rob1)
        world.initialize(False)

        l = list(world.enumerate_fluent_instances(world.getFluent("xpos")))
        self.assertListEqual([['rob1']], l)

        l = list(world.enumerate_fluent_instances(world.getFluent("carrying")))
        self.assertTrue(['rob1', 'coffee'] in l)
        self.assertTrue(['rob1', 'chocolate'] in l)
        self.assertEqual(len(l), 2)

        l = list(world.enumerate_fluent_instances(world.getFluent("time")))
        self.assertEqual(len(l), 1)
        self.assertListEqual([[]], l)

    @withHeader
    def testCheckFluentInitialization(self):
        world = World.instance()
        rob1 = Agent("rob1", "robot", Procedure("main", [], Sequence([])))
        world.addAgent(rob1)
        world.initialize(False)

        expected_fluents = [('ypos', ['rob1']), ('carrying', ['rob1', 'coffee']), ('carrying', ['rob1', 'chocolate']),
                            ('xpos', ['rob1']), ('painted', ['coffee']), ('painted', ['chocolate']),
                            ("active", ["rob1"])]
        expected_constants = [('gravity', []), ('robot_radius', ['rob1'])]

        l1, l2 = world.check_fluent_initialization()
        for e in expected_fluents:
            self.assertTrue(e in l1)
        for e in expected_constants:
            self.assertTrue(e in l2)
        print("l1 = " + str(l1))
        self.assertEqual(len(l1), 7)
        self.assertEqual(len(l2), 2)

        world.setFluentValue('xpos', ['rob1'], 10)
        expected_fluents.remove(("xpos", ["rob1"]))
        l1, l2 = world.check_fluent_initialization()
        for e in expected_fluents:
            self.assertTrue(e in l1)
        for e in expected_constants:
            self.assertTrue(e in l2)
        self.assertEqual(len(l1), 6)
        self.assertEqual(len(l2), 2)

        world.setFluentValue('carrying', ['rob1', 'coffee'], True)
        expected_fluents.remove(('carrying', ['rob1', 'coffee']))
        l1, l2 = world.check_fluent_initialization()
        for e in expected_fluents:
            self.assertTrue(e in l1)
        for e in expected_constants:
            self.assertTrue(e in l2)
        self.assertEqual(len(l1), 5)
        self.assertEqual(len(l2), 2)

        world.setConstantValue('gravity', [], 9.81)
        expected_constants.remove(("gravity", []))
        l1, l2 = world.check_fluent_initialization()
        for e in expected_fluents:
            self.assertTrue(e in l1)
        for e in expected_constants:
            self.assertTrue(e in l2)
        self.assertEqual(len(l1), 5)
        self.assertEqual(len(l2), 1)

        world.setConstantValue('robot_radius', ['rob1'], 20.0)
        expected_constants.remove(("robot_radius", ["rob1"]))
        l1, l2 = world.check_fluent_initialization()
        for e in expected_fluents:
            self.assertTrue(e in l1)
        for e in expected_constants:
            self.assertTrue(e in l2)
        self.assertEqual(len(l1), 5)
        self.assertEqual(len(l2), 0)


class WorldTestSuite(unittest.TestSuite):
    def __init__(self):
        unittest.TestSuite.__init__(self)
        self.addTest(WorldTest("test_procedure_call"))


def suite():
    s = unittest.TestSuite()
    s.addTest(WorldTest('test_procedure_call'))
    s.addTest(WorldTest('test_recursive_procedure_call'))
    return s

def load_tests(loader, tests, pattern):
    print("load")
    return suite()

if __name__ == '__main__':
    unittest.main()