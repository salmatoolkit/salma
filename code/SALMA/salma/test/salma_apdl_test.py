import math

from salma import constants
from salma.constants import SELF
from salma.model.agent import Agent
from salma.model.core import Entity
from salma.experiment import Experiment
from salma.model.process import OneShotProcess
from salma.model.selectionstrategy import OutcomeSelectionStrategy, Uniform
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, While, Act, Wait, Procedure, Sequence, Assign, FunctionControlNode, \
    Variable, \
    Select, If, Iterate, makevars
from salma.model.world import World
from salma.test.testhelpers import withHeader
from salma.test.world_test_base import BaseWorldTest


def print_value(value):
    print("Val: ", value)
    return ControlNode.CONTINUE, None


def create_value_recorder(reclist: list):
    def rfunc(*params, **kwargs):
        reclist.append(params)

    return rfunc


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


class SALMAAPDLTest(BaseWorldTest):
    def setupSelectionContext(self, carry_map):
        world = World.instance()

        for i in range(1, 11):
            item = Entity("item" + str(i), "item")
            world.addEntity(item)

        world.initialize(False)
        self.initialize_robot("rob1", 10, 10, 0, 0)
        self.initialize_robot("rob2", 10, 20, 0, 0)
        self.initialize_items()
        for rob, items in carry_map.items():
            for i in items:
                world.setFluentValue("carrying", [rob, "item" + str(i)], True)

    def testSelect_Fluent(self):
        world = World.instance()
        reclist1 = []
        recorder1 = create_value_recorder(reclist1)
        reclist2 = []
        recorder2 = create_value_recorder(reclist2)
        r, i = makevars(("r", "robot"), ("i", "item"))
        agent1 = Agent("rob1", "robot", Procedure([
            Select("carrying", [r, i]),
            FunctionControlNode(recorder1, r, i),
            Act("mark", [SELF, i, SELF])
        ]))
        agent2 = Agent("rob2", "robot", Procedure([
            Select("carrying", [SELF, i]),
            FunctionControlNode(recorder2, i),
            Act("drop", [SELF, i])
        ]))

        world.addAgent(agent1)
        world.addAgent(agent2)
        carry_map = {"rob1": [3, 5, 9, 2, 10], "rob2": [1, 4, 6, 7, 8]}
        self.setupSelectionContext(carry_map)

        print("\n\n----\n\n")
        print("INIT:")
        print("\n\n----\n\n")
        world.printState()
        experiment = Experiment(world)
        verdict, results = experiment.run_until_finished()
        print(reclist1)
        print(reclist2)
        world.printState()
        self.assertEqual(len(reclist1), 1)
        r, i = reclist1[0]
        self.assertTrue(int(i[4:]) in carry_map[r])
        self.assertEqual(world.getFluentValue("marking", [i]), r)

        self.assertEqual(len(reclist2), 1)
        i = reclist2[0][0]
        self.assertTrue(int(i[4:]) in carry_map["rob2"])
        self.assertFalse(world.getFluentValue("carrying", [agent2.id, i]))

    def testIterate_Fluent(self):
        world = World.instance()
        reclist1 = []
        recorder1 = create_value_recorder(reclist1)
        reclist2 = []
        recorder2 = create_value_recorder(reclist2)
        r, i = makevars(("r", "robot"), ("i", "item"))
        agent1 = Agent("rob1", "robot", Procedure([
            Iterate("carrying", [r, i], [
                FunctionControlNode(recorder1, r, i),
                Act("mark", [SELF, i, SELF])])
        ]))
        agent2 = Agent("rob2", "robot", Procedure([
            Iterate("carrying", [SELF, i], [
                FunctionControlNode(recorder2, i),
                Act("paint", [SELF, i])])
        ]))

        world.addAgent(agent1)
        world.addAgent(agent2)
        carry_map = {"rob1": [3, 5, 9, 2, 10], "rob2": [1, 4, 6, 7, 8]}
        self.setupSelectionContext(carry_map)

        print("\n\n----\n\n")
        print("INIT:")
        print("\n\n----\n\n")
        world.printState()
        experiment = Experiment(world)
        verdict, results = experiment.run_until_finished()
        print(reclist1)
        print(reclist2)
        world.printState()

