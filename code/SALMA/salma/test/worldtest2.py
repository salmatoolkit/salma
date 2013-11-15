import logging
import unittest

from salma import constants
from salma.SMCException import SMCException
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
from salma.test.testhelpers import withHeader


def printValue(value):
    print("Val: ", value)
    return (ControlNode.CONTINUE, None)


        
        

class WorldTest2(unittest.TestCase):
    '''
    Conducts experiments with a simple scenario whose distribution can be given in closed form.  
    '''
    
    P_COLLISION = 0
    P_DROP = 0.02
    TIME_LIMIT = 20
    SAMPLE_LENGTH = 10
    SAMPLES = 10
    
    
    
    @classmethod
    def setUpClass(cls):
        try:
            World.set_logic_engine(EclipseCLPEngine("../test/domaindesc.ecl"))
        except SMCException as e:
            print(e)
            raise
        logger = logging.getLogger('agamemnon-smc')
        logger.setLevel(logging.INFO)
        ch = logging.StreamHandler()
        logger.addHandler(ch)
        
    def setUp(self):
        World.create_new_world()
        world = World.instance()
        world.addFluent(Fluent("xpos", "integer", [("r", "robot")], range=(0, 1000)))
        world.addFluent(Fluent("ypos", "integer", [("r", "robot")], range=(0, 1000)))

        world.addFluent(Fluent("carrying", "boolean", [("r", "robot"), ("i", "item")]))
        
        world.addFluent(Fluent("active", "boolean", ["r", "robot"]))
        world.addAction(DeterministicAction('move_right', [('r', 'robot')]))
        world.addAction(DeterministicAction('move_down', [('r', 'robot')]))
        world.addAction(DeterministicAction('grab', [('r', 'robot'), ('i', 'item')]))
        world.addAction(DeterministicAction('drop', [('r', 'robot'), ('i', 'item')]))
        
        dropEvent = ExogenousAction('accidental_drop',
                                      [
                                       ('r', 'robot'),
                                       ('i', 'item')
                                      ],
                                      BernoulliDistribution(WorldTest2.P_DROP),
                                      []                                     
                                      )
        
        collisionEvent = ExogenousAction('collision',
                                      [
                                       ('r1', 'robot'),
                                       ('r2', 'robot')
                                      ],
                                      BernoulliDistribution(WorldTest2.P_COLLISION),
                                      [('severity', UniformDistribution('integer', valueRange=(0, 100)))]
                                      )
        
        
        world.addExogenousAction(dropEvent)
        world.addExogenousAction(collisionEvent)
        
        
    def __placeAgentsInColumn(self, x):
        world = World.instance()
        y = 10
             
        # : :type r: Agent 
        for r in world.getDomain('robot'):    
            world.setFluentValue("xpos", [r.getId()], x)
            world.setFluentValue("ypos", [r.getId()], y)
            world.setFluentValue("active", [r.getId()], True)
            y += 20
    
    
    def createRightMovingRobot(self, num):
        '''
        Creates a simple agent that grabs an item wit id item+num and keeps moving right as long as the agent is active 
        '''
        
        mainSeq = Sequence()
        
        innerSeq = Sequence()
        innerSeq.addChild(ActionExecution("move_right", [Entity.SELF]))
        
        
        mainSeq.addChild(ActionExecution("grab", [Entity.SELF, "item" + str(num)]))
        mainSeq.addChild(
                         While(EvaluationContext.PYTHON_EXPRESSION,
                               "True",
                               [Entity.SELF],
                               innerSeq
                               )
                         )
  
      
        
        agent = Agent("rob" + str(num), "robot", Procedure("main", [], mainSeq))
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
        
        
        for i in range(1,3):    
            world.addEntity(Entity("item" + str(i), "item"))
            world.addAgent(self.createRightMovingRobot(i))
            
        world.initialize(False)
        
        self.__placeAgentsInColumn(10)
        self.setNoOneCarriesAnything()
        
        world.registerProperty("f", ("forall([r,robot], "
                                        "forall([i,item], "
                                            "implies("
                                                "occur(grab(r,i)),"
                                                "until(25,"
                                                    "carrying(r,i),"
                                                    "xpos(r) > 20)"
                                                    ")"
                                                ")"
                                            ")"))
          
        
        
        
        #world.registerProperty("f", "forall([r,robot], xpos(r) > 1)")
#         
        #world.registerProperty("f", "xpos(rob1) > 50")
        #world.registerProperty("f", "until(25, xpos(rob1) > 50")
        
        
        successes = []
        
        for i in range(0,WorldTest2.SAMPLES):
            res = world.runRepetitions(WorldTest2.SAMPLE_LENGTH)
            num = sum(res)
            successes.append(num)
            print("Run #{}: {}".format(i, num))
            
        print("Successes:")
        print(successes)
        
        
        
     
if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()   
      
