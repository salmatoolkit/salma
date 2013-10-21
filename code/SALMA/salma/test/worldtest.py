import logging
import unittest
from scipy.odr.__odrpack import odr_stop

from salma import constants
from salma.SMCException import SMCException
from salma.engine import EclipseCLPEngine
from salma.model import procedure, distributions
from salma.model.core import Agent, Entity, Fluent, Action, \
    DeterministicAction, StochasticAction, StepwiseStochasticAction, \
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


class WorldTest(unittest.TestCase):
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
        world.addFluent(Fluent("xpos", "int", [("r", "robot")], range=(0, 1000)))
        world.addFluent(Fluent("ypos", "int", [("r", "robot")], range=(0, 1000)))

        world.addFluent(Fluent("carrying", "bool", [("r", "robot"), ("i", "item")]))
        world.addFluent(Fluent("painted", "bool", [("i", "item")]))

        world.addConstant(Constant('gravity', 'float', []))
        world.addConstant(Constant('robot_radius', 'float', [('r', 'robot')]))
        world.addAction(DeterministicAction('move_right', [('r', 'robot')]))
        world.addAction(DeterministicAction('move_down', [('r', 'robot')]))
        world.addAction(DeterministicAction('grab', [('r', 'robot'), ('i', 'item')]))
        world.addAction(DeterministicAction('drop', [('r', 'robot'), ('i', 'item')]))
        world.addAction(DeterministicAction('paint', [('r', 'robot'), ('i', 'item')]))

        world.addEntity(Entity("coffee", "item"))
        world.addEntity(Entity("chocolate", "item"))

    #     def tearDown(self):
    #         World.logicsEngine.cleanup()

    def createRightMovingRobot(self, robotId):
        seq = Sequence()
        seq.addChild(ActionExecution("move_right", [Entity.SELF]))
        seq.addChild(ActionExecution("move_down", [Entity.SELF]))
        agent = Agent(robotId, "robot", Procedure("main", [], seq))
        return agent


    def setNoOneCarriesAnything(self):
        world = World.getInstance()
        robots = world.getDomain('robot')
        items = world.getDomain('item')
        for r in robots:
            for i in items:
                world.setFluentValue('carrying', [r.id, i.id], False)


    @withHeader
    def testWorldStepExplicit(self):
        world = World.getInstance()
        world.addAgent(self.createRightMovingRobot('rob1'))
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
        world = World.getInstance()
        world.addAgent(self.createRightMovingRobot('rob1'))
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
        world = World.getInstance()

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
        world = World.getInstance()

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
        world = World.getInstance()

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
        self.assertEqual(agent.evaluationContext.resolve(Variable('myY'))[0], 20)
        self.assertEqual(agent.evaluationContext.resolve(Variable('myX'))[0], 20)

    @withHeader
    def testEvaluate_PythonExression(self):
        world = World.getInstance()

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

        self.assertEqual(agent.evaluationContext.resolve(Variable("x"))[0], 6)
        self.assertEqual(agent.evaluationContext.resolve(Variable("y"))[0], 45)
        self.assertEqual(agent.evaluationContext.resolve(Variable("z"))[0], 42)
        self.assertEqual(agent.evaluationContext.resolve(Variable("z2"))[0], 25)


    @withHeader
    def testRandomizeFluents(self):
        world = World.getInstance()
        world.addAgent(self.createRightMovingRobot('rob3'))
        world.addAgent(self.createRightMovingRobot('rob4'))

        for i in range(1, 10):
            print("Try {}".format(i))
            print("-" * 50)
            world.reset()
            world.sampleFluentValues()
            world.printState()


    def generateOutcomes(self):
        outcome1 = RandomActionOutcome('land_on',
                                       [
                                           ('rob', ArgumentIdentityDistribution(0)),
                                           ('x', UniformDistribution('int', (100, 500))),
                                           ('y', UniformDistribution('int', (0, 200)))
                                       ])

        outcome2 = RandomActionOutcome('crash',
                                       [
                                           ('rob', ArgumentIdentityDistribution(0))
                                       ])
        return (outcome1, outcome2)

    @withHeader
    def testUniformStochasticAction(self):
        world = World.getInstance()

        outcome1, outcome2 = self.generateOutcomes()

        jumpAction = StepwiseStochasticAction('jump',
                                              [
                                                  ('rob', 'robot'),
                                                  ('height', 'int')
                                              ],
                                              [
                                                  (0.5, outcome1),
                                                  (0.5, outcome2)
                                              ]
        )

        world.addAction(jumpAction)

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
        world = World.getInstance()

        outcome1, outcome2 = self.generateOutcomes()

        def sampler(evaluationContext, paramValues):
            x = evaluationContext.getFluentValue('xpos', paramValues[0])
            height = paramValues[1]
            if x > 100 or height > 50:
                return outcome2
            else:
                return outcome1


        jumpAction = StochasticAction('jump',
                                      [
                                          ('rob', 'robot'),
                                          ('height', 'int')
                                      ],
                                      sampler
        )

        world.addAction(jumpAction)

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


    def __placeAgentsInColumn(self, x=10, startY=10, distance=20):
        world = World.getInstance()

        y = startY

        # : :type r: Agent 
        for r in world.getDomain('robot'):
            world.setFluentValue("xpos", [r.getId()], x)
            world.setFluentValue("ypos", [r.getId()], y)
            world.setFluentValue("active", [r.getId()], True)
            y += distance


    @withHeader
    def testExogenousAction(self):

        world = World.getInstance()  # : :type world: World 

        dropEvent = ExogenousAction('accidental_drop',
                                    [
                                        ('r', 'robot'),
                                        ('i', 'item')
                                    ],
                                    BernoulliDistribution(0.7),
            []
        )

        collisionEvent = ExogenousAction('collision',
                                         [
                                             ('r1', 'robot'),
                                             ('r2', 'robot')
                                         ],
                                         BernoulliDistribution(0.7),
                                         [('severity', UniformDistribution('int', valueRange=(0, 100)))]
        )

        world.addExogenousAction(dropEvent)
        world.addExogenousAction(collisionEvent)

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

        self.__placeAgentsInColumn()
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
        world = World.getInstance()
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

        self.__placeAgentsInColumn()

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
        world = World.getInstance()

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

        agent1 = Agent("rob1", "robot", Procedure("main", [], seq1))
        agent2 = Agent("rob2", "robot", Procedure("main", [], seq2))

        grabMap = {"rob1": item1, "rob2": item2}
        world.addAgent(agent1)
        world.addAgent(agent2)

        world.initialize(False)

        self.__placeAgentsInColumn()

        self.setNoOneCarriesAnything()

        return agent1, agent2, grabMap

    @withHeader
    def testSelectAll(self):
        world = World.getInstance()
        agent1, agent2, grabMap = self.setupSelectionContext()
        res1 = agent1.evaluationContext.selectAll(EvaluationContext.FLUENT, "carrying",
                                                  ('r', 'robot'), ('i', 'item'))
        self.assertListEqual(res1, [])
        world.runUntilFinished()

        res2 = agent1.evaluationContext.selectAll(EvaluationContext.FLUENT, "carrying",
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
        world = World.getInstance()
        agent1, agent2, grabMap = self.setupSelectionContext()
        res1 = agent1.evaluationContext.selectFirst(EvaluationContext.FLUENT, "carrying",
                                                    ('r', 'robot'), ('i', 'item'))
        self.assertIsNone(res1)
        world.runUntilFinished()

        res2 = agent1.evaluationContext.selectFirst(EvaluationContext.FLUENT, "carrying",
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
        world = World.getInstance()
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
        world = World.getInstance()

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
                         agent.evaluationContext.getEntity(
                             agent.evaluationContext.resolve(Variable("i", "item"))[0]
                         )
        )


    @withHeader
    def testProcedureCall(self):
        world = World.getInstance()
        transportToX = Procedure("transportToX",
                                 [("r1", "robot"), ("i", "item"), ("targetX", "int")],
                                 Sequence(
                                     [
                                         ActionExecution("grab", [Variable("r1"), Variable("i")]),
                                         While(EvaluationContext.TRANSIENT_FLUENT, "robotLeftFrom",
                                               [Variable("r1"), Variable("targetX")],
                                               ActionExecution("move_right",
                                                               [Variable("r1")]
                                               )
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

        self.__placeAgentsInColumn(10)

        world.runUntilFinished()
        world.printState()
        self.assertEqual(world.getFluentValue('xpos', ['rob1']), 25)

    @withHeader
    def testCreatePLan_OK_Unique(self):
        world = World.getInstance()
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

        self.__placeAgentsInColumn(10)

        world.runUntilFinished()
        world.printState()

        resolvedValues = agent.evaluationContext.resolve(
            Variable("r"),
            Variable("i"),
            Variable("plan"))
        print("resolvedValues:", resolvedValues)

    @withHeader
    def testEnumerateFluentInstances(self):
        world = World.getInstance()
        rob1 = Agent("rob1", "robot", Procedure("main", [], Sequence([])))
        world.addAgent(rob1)
        world.initialize(False)

        l = list(world.enumerateFluentInstances(world.getFluent("xpos")))
        self.assertListEqual([['rob1']], l)

        l = list(world.enumerateFluentInstances(world.getFluent("carrying")))
        self.assertTrue(['rob1', 'coffee'] in l)
        self.assertTrue(['rob1', 'chocolate'] in l)
        self.assertEqual(len(l), 2)


    @withHeader
    def testCheckFluentInitialization(self):
        world = World.getInstance()
        rob1 = Agent("rob1", "robot", Procedure("main", [], Sequence([])))
        world.addAgent(rob1)
        world.initialize(False)

        l = world.checkFluentInitialization()
        expected = [('ypos', ['rob1']), ('carrying', ['rob1', 'coffee']), ('carrying', ['rob1', 'chocolate']),
                    ('xpos', ['rob1']), ('painted', ['coffee']), ('painted', ['chocolate']),
                    ('gravity', []), ('robot_radius', ['rob1'])]
        for e in expected:
            self.assertTrue(e in l)

        self.assertEqual(len(l), len(expected))
        world.setFluentValue('xpos', ['rob1'], 10)
        l = world.checkFluentInitialization()
        expected = [('ypos', ['rob1']), ('carrying', ['rob1', 'coffee']), ('carrying', ['rob1', 'chocolate']),
                    ('painted', ['coffee']), ('painted', ['chocolate']),
                    ('gravity', []), ('robot_radius', ['rob1'])]
        for e in expected:
            self.assertTrue(e in l)
        self.assertEqual(len(l), len(expected))

        world.setFluentValue('carrying', ['rob1', 'coffee'], True)
        l = world.checkFluentInitialization()
        expected = [('ypos', ['rob1']), ('carrying', ['rob1', 'chocolate']),
                    ('painted', ['coffee']), ('painted', ['chocolate']),
                    ('gravity', []), ('robot_radius', ['rob1'])]
        for e in expected:
            self.assertTrue(e in l)
        self.assertEqual(len(l), len(expected))

        world.setConstantValue('gravity', [], 9.81)
        l = world.checkFluentInitialization()
        expected = [('ypos', ['rob1']), ('carrying', ['rob1', 'chocolate']),
                    ('painted', ['coffee']), ('painted', ['chocolate']),
                    ('robot_radius', ['rob1'])]
        for e in expected:
            self.assertTrue(e in l)
        self.assertEqual(len(l), len(expected))

        world.setConstantValue('robot_radius', ['rob1'], 20.0)
        l = world.checkFluentInitialization()
        expected = [('ypos', ['rob1']), ('carrying', ['rob1', 'chocolate']),
                    ('painted', ['coffee']), ('painted', ['chocolate'])]
        for e in expected:
            self.assertTrue(e in l)
        self.assertEqual(len(l), len(expected))

# def suite():
#     suite = unittest.TestSuite()
#     suite.addTest(WorldTest('testRandomizeFluents'))
#     suite.addTest(WorldTest('testRunRightUntilMaxXPos'))
#      
#     return suite
#   
#            
# def load_tests(loader, tests, pattern):
#     print(loader, tests, pattern)
#     return suite()

