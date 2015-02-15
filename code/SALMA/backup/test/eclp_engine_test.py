from pyclp import *
import unittest
from salma.model import core
from salma.engine import *
from salma.test.testhelpers import withHeader
import pprint
import os
class Robot(core.Agent):
    def __init__(self):
        pass

def printState(engine):
    fluents = engine.getCurrentState()

    for f in fluents:
        print(f )



class EclipseCLPEngineTest(unittest.TestCase):
    
    def setUp(self):
        print(os.getcwd())
        self.engine = EclipseCLPEngine("../ecl-test/domaindesc.ecl", "../ecl-test/example_procedures.ecl")
        self.robots = ["rob1", "rob2", "rob3"]
        self.items = ["item1", "item2", "item3"]
        self.engine.defineDomain("robot", self.robots)
        self.engine.defineDomain("item", self.items)

        x = 100
        y = 100
        self.engine.setFluentValue("time",[],0)

        for r in self.robots:
            self.engine.setFluentValue("xpos",[r],x)
            self.engine.setFluentValue("ypos",[r],y)
            self.engine.setFluentValue("velocity_x",[r],0)
            self.engine.setFluentValue("velocity_y",[r],0)
            x += 50
            y += 50
            for i in self.items:
                self.engine.setFluentValue("carrying", [r, i], False)
                
            
            
    @withHeader()
    def testProgression(self):
        print("BEFORE:\n\n")
        printState(self.engine)
        
        failedActions = self.engine.progress([
                              ('move_right',['rob1']),
                              ("move_left",["rob2"]),
                              ("grab",["rob1","item1"]),
                              ("tick",[1])
                              ])                    
        
        print("AFTER:\n\n")
        printState(self.engine)
        
        print("Failed:\n")
        print(failedActions)

    @withHeader()
    def testEvaluateCondition(self):
        print("\n\nTry 1:")
        result = self.engine.evaluateCondition("robotLeftFrom", "rob1", 100, situation = 's0')
        print("result: ", result)
        self.engine.progress([("move_left",["rob1"])])  
        print("\n\nTry 2:")
        result = self.engine.evaluateCondition("robotLeftFrom", "rob1", 100, situation = 's0')
        print("result: ", result)
        result = self.engine.evaluateCondition("robotRightFrom", "rob1", 0, situation = 's0')

        print("result3: ", result)

    @withHeader()
    def testEvaluationStep(self):
        self.engine.registerProperty('f', 'xpos(rob1) > 20')
        self.engine.registerProperty('g', 'xpos(rob1) > 200')
        self.engine.registerProperty('h', 'until(10, xpos(rob1) > 20, xpos(rob1) > 200)')
        self.engine.registerProperty('i', 'until(2, xpos(rob1) > 20, xpos(rob1) > 200)')
        print(self.engine.printToplevelGoals())
        print(self.engine.printToplevelGoals())
        
        
        verdict, toplevel, scheduled, _ = self.engine.evaluationStep()
  
        print("Verdict: {}\nToplevel:{}\nScheduled:{}".format(verdict, str(toplevel), str(scheduled)))
        
        # skip
        self.engine.evaluationStep()
        self.engine.evaluationStep()
        
        
        verdict, toplevel, scheduled, _ = self.engine.evaluationStep()
        print("Verdict: {}\nToplevel:{}\nScheduled:{}".format(verdict, str(toplevel), str(scheduled)))
        
        
        
    @withHeader()
    def testGetExogenousActionCandidates(self):
        candidates = self.engine.get_currently_possible_ad_hoc_event_instances()
        print("BEFORE:\n")
        print("-" * 20)
        pprint.pprint(candidates)
        
        self.engine.setFluentValue("carrying", ["rob1", "item1"] , True)
        
        candidates = self.engine.get_currently_possible_ad_hoc_event_instances()
        print("AFTER:")
        print("-" * 20)
        pprint.pprint(candidates)
        
        
     
    @withHeader()
    def testSelectAll(self):   
        self.engine.setFluentValue("carrying", ['rob1', 'item1'], True)
        self.engine.setFluentValue("carrying", ['rob3', 'item3'], True)
        result = self.engine.selectAll("carrying", ('rob', 'robot'), ('i', 'item'), situation = 's0')
        self.assertListEqual(
                result, 
                [{'i': 'item1', 'rob': 'rob1'}, {'i': 'item3', 'rob': 'rob3'}]
        )
        
        pprint.pprint(result)
        
    @withHeader()
    def testSelectFirst(self):   
        self.engine.setFluentValue("carrying", ['rob1', 'item1'], True)
        self.engine.setFluentValue("carrying", ['rob3', 'item3'], True)
        result = self.engine.selectFirst("carrying", ('rob', 'robot'), ('i', 'item'), situation = 's0')
        self.assertDictEqual(result, {'i': 'item1', 'rob': 'rob1'}) 
        pprint.pprint(result)
    
    @withHeader()
    def testCreatePlan_OK_UniquePlan(self):
        
        self.engine.setFluentValue("carrying", ['rob1', 'item1'], True)
        self.engine.setFluentValue("carrying", ['rob2', 'item3'], True)
        plan, values = self.engine.createPlan("transportToX", 
                                              ("rob", "robot"),
                                              ("i", "item"),
                                              207)
        print("Plan:")
        for action in plan:
            print(action)
        
        
        print("\n\nValues:", values)
           
        printState(self.engine)
        
        self.assertEqual(values['rob'],'rob3')
        self.assertEqual(values['i'],'item2')
        self.assertListEqual(plan,
                             [('grab', 'rob3', 'item2'),
                              ('move_right', 'rob3'),
                              ('move_right', 'rob3'),
                              ('move_right', 'rob3'),
                              ('move_right', 'rob3'),
                              ('move_right', 'rob3'),
                              ('move_right', 'rob3'),
                              ('move_right', 'rob3'),
                              ('drop', 'rob3', 'item2')]
                            )
        
    @withHeader()
    def testCreatePlan_None(self):
        # make all robots carry some item so our brilliant plan will just not work
        self.engine.setFluentValue("carrying", ['rob1', 'item1'], True)
        self.engine.setFluentValue("carrying", ['rob2', 'item2'], True)
        self.engine.setFluentValue("carrying", ['rob3', 'item3'], True)
        plan, values = self.engine.createPlan("transportToX", 
                                              ("rob", "robot"),
                                              ("i", "item"),
                                              207)
        
        self.assertIsNone(plan)
        self.assertIsNone(values)


    @withHeader()
    def testEvaluateFunctionGoal(self):
        v = self.engine.evaluateFunctionGoal('times',6,7)
        self.assertEquals(v, 42)

    @withHeader()
    def testConstants_Unititialized(self):
        for r in self.robots:
            self.assertFalse(self.engine.isConstantDefined('robotRadius', [r]))
            self.assertIsNone(self.engine.getConstantValue('robotRadius', [r]))
    
    @withHeader()
    def testManipulateConstants_OK(self):
        self.engine.setConstantValue('robotRadius', ['rob1'], 12.3)
        self.engine.setConstantValue('gravity', [], 9.81)
        
        self.assertEqual(self.engine.getConstantValue('robotRadius', ['rob1']), 12.3)
        self.assertEqual(self.engine.getConstantValue('gravity', []), 9.81)
        
        v1 = self.engine.evaluateFunctionGoal('robotRadius', 'rob1')
        self.assertEqual(v1, 12.3)
        v2 = self.engine.evaluateFunctionGoal('gravity')
        self.assertEqual(v2, 9.81)

    def tearDown(self):
        self.engine.cleanup()             



