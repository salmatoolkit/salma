import unittest
from salma.model import procedure
from salma.model.procedure import Statement
from salma.model.evaluationcontext import EvaluationContext

handler1Count = 0
handler2Count = 0
handler3Count = 0


def handler1():
    global handler1Count
    print("handler1 executed")
    handler1Count += 1
    return Statement.CONTINUE


def handler2():
    global handler2Count
    print("handler2 executed")
    handler2Count += 1
    return Statement.CONTINUE


def handler3():
    global handler3Count
    print("handler3 executed")
    handler3Count += 1
    return Statement.BLOCK


class DummyEvaluationContext(EvaluationContext):
    def __init__(self):
        EvaluationContext.__init__(self)


class Test(unittest.TestCase):
    def testSequenceCreatedCorrectly(self):
        global handler1Count, handler2Count, handler3Count
        ec = DummyEvaluationContext()

        seq = procedure.Sequence()
        seq.add_child(procedure.FunctionStatement(handler1))
        seq.add_child(procedure.FunctionStatement(handler2))
        seq.add_child(procedure.FunctionStatement(handler1))

        handler1Count = handler2Count = handler3Count = 0

        (state, nextNode) = seq.execute_step(ec)
        self.assertEqual(Statement.CONTINUE, state)
        self.assertEqual(seq, nextNode)
        self.assertEqual(1, handler1Count)
        self.assertEqual(0, handler2Count)

        (state, nextNode) = seq.execute_step(ec)
        self.assertEqual(Statement.CONTINUE, state)
        self.assertEqual(seq, nextNode)
        self.assertEqual(1, handler1Count)
        self.assertEqual(1, handler2Count)

        (state, nextNode) = seq.execute_step(ec)
        self.assertEqual(Statement.CONTINUE, state)
        self.assertEqual(seq, nextNode)
        self.assertEqual(2, handler1Count)
        self.assertEqual(1, handler2Count)

        (state, nextNode) = seq.execute_step(ec)
        self.assertEqual(Statement.CONTINUE, state)
        self.assertEqual(None, nextNode)
        self.assertEqual(2, handler1Count)
        self.assertEqual(1, handler2Count)


    def testArbitraryActionExecutedCorrectly(self):
        global handler1Count
        handler1Count = 0
        ec = DummyEvaluationContext()
        action = procedure.FunctionStatement(handler1)
        print("id: ", action.id)
        (a, b) = action.execute_step(ec)
        self.assertEqual(a, Statement.CONTINUE)
        self.assertEqual(b, None)
        self.assertEqual(1, handler1Count)


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()