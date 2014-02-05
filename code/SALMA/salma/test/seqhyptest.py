'''
Created on 05.09.2013

@author: christian
'''
import logging
import unittest
import time

from salma import constants
from salma.SALMAException import SALMAException
from salma.engine import EclipseCLPEngine, EclipseCLPEngine
from salma.model import procedure, distributions
from salma.model.core import Agent, Entity, Fluent, Action, \
    DeterministicAction, StochasticAction, UniformStochasticAction, \
    RandomActionOutcome, ExogenousAction
from salma.model.distributions import UniformDistribution, \
    ArgumentIdentityDistribution, BernoulliDistribution, Distribution
from salma.model.evaluationcontext import EvaluationContext
from salma.model.procedure import ControlNode, Sequence, \
    ActionExecution, Procedure, While, VariableAssignment, ArbitraryAction, Variable
from salma.model.world import World
import salma.model.world
from salma.statistics import SequentialAcceptanceTest
import numpy as np
from salma.test.testhelpers import withHeader


# global constants
P_COLLISION = 0
P_DROP = 0.01
TIME_LIMIT = 20
SAMPLE_LENGTH = 100
SAMPLES = 100


# probability that one experiment fails: 1 - P(all agents finish without drop) 
# since number of steps for each agent = 10:
P_DEFECT = 1 - ((1-P_DROP)**10)**3
TOLERANCE = 0.05
STEPS = 200

# null hypothesis is that probability of defect is at most P0
# clearly an error of second kind (accept H0 if P > P1) is worse here
ALPHA = 0.05
BETA = 0.05

class SequentialHypothesisTestTest(unittest.TestCase):


    
    
    
    @classmethod
    def setUpClass(cls):
        domainFileName = "kroiss_aamas2014_domaindesc.ecl"
        try:
            World.set_logic_engine(EclipseCLPEngine("../test/" + domainFileName))
        except SALMAException as e:
            print(e)
            raise
        logger = logging.getLogger('agamemnon-smc')
        logger.setLevel(logging.INFO)
        ch = logging.StreamHandler()
        logger.addHandler(ch)
        
    def setUp(self):
        World.create_new_world()
        world = World.instance()
        world.addFluent(Fluent("xpos", "integer", [("o", "object")], range=(0, 1000)))

        world.addFluent(Fluent("carrying", "boolean", [("r", "robot"), ("i", "item")]))
        
        world.addAction(DeterministicAction('move_right', [('r', 'robot')]))
        world.addAction(DeterministicAction('grab', [('r', 'robot'), ('i', 'item')]))
        
        dropEvent = ExogenousAction('accidental_drop',
                                      [
                                       ('r', 'robot'),
                                       ('i', 'item')
                                      ],
                                      BernoulliDistribution(P_DROP),
                                      []                                     
                                      )        
        
        world.addExogenousAction(dropEvent)
        
        
        
    def __placeAgentsInColumn(self, x):
        world = World.instance()
        # y = 10
             
        # : :type r: Agent 
        for r in world.getDomain('robot'):    
            world.setFluentValue("xpos", [r.getId()], x)
    
    
    def createRightMovingRobot(self, num):
        '''
        Creates a simple agent that grabs an item wit id item+num and keeps moving right as long as the agent is active 
        '''
        
        mainSeq = Sequence([
                ActionExecution("grab", [Entity.SELF, "item" + str(num)]),
                While(EvaluationContext.PYTHON_EXPRESSION, "True",
                   [Entity.SELF],
                   ActionExecution("move_right", [Entity.SELF])
                )])
  
      
        
        agent = Agent("rob" + str(num), "robot", Procedure("main",[],mainSeq))
        return agent
    
    
    def setNoOneCarriesAnything(self):
        world = World.instance()
        robots = world.getDomain('robot')
        items = world.getDomain('item')
        for r in robots:
            for i in items:
                world.setFluentValue('carrying', [r.id, i.id], False)
    
                
        
   
    
         
    @withHeader
    def testScenario(self):
        
        world = World.instance()  # : :type world: World
        
        
        for i in range(1,4):    
            itemid = "item" + str(i)
            world.addEntity(Entity(itemid, "item"))
           
            world.addAgent(self.createRightMovingRobot(i))
            
        world.initialize(False)
        for i in range(1,4):    
            itemid = "item" + str(i)
            world.setFluentValue('xpos', [itemid], 10)
        
        self.__placeAgentsInColumn(10)
        self.setNoOneCarriesAnything()
        f = '''
        forall([r,robot],
            forall([i,item],
                implies(
                    occur(grab(r,i)),
                    until(12,
                        carrying(r,i),
                        xpos(i) > 20
                    )
                )
            )
        )
        '''
        world.registerProperty("f", f)
          
        
        #world.registerProperty("f", ("forall([r,robot], xpos(r) > 100)"))
#         world.registerProperty("f", "forall([r,robot], xpos(r) > 50)")
#         
        #world.registerProperty("f", "xpos(rob1) > 50")

        print("P_DEFECT: {}".format(P_DEFECT))
    
       
        p0s = np.linspace(0.001, 0.999 - TOLERANCE, STEPS, True)
        p1s = p0s + TOLERANCE
        data = np.array([np.arange(STEPS) + 1, p0s, p1s, np.repeat(None, STEPS), np.zeros(STEPS), np.zeros(STEPS)]).T
        print(f)
        world.printState()
        #print(data)
        for row in data:
            c1 = time.clock()       
            result, m = world.runSequentialHypothesisTest(row[1], row[2], ALPHA, BETA)
            c2 = time.clock()
            row[3:] = [result, m, (c2-c1)]
            print(row)                                 
        
        # header: P_DEFECT
        np.savetxt('seqtest_aamas2014.csv', data, ["%d","%5f","%5f", "%s", "%d","%5f"], delimiter=';',
                   header="{:5f};{:5f};{:5f};{:5f}".format(P_DEFECT, TOLERANCE, ALPHA, BETA))
        
        


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testName']
    unittest.main()