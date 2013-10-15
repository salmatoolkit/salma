'''
Created on 30.07.2013

@author: kroiss
'''
import unittest
from salma.test.testhelpers import withHeader
from salma.model.core import ExogenousAction, Agent, Entity
from salma.model.procedure import Procedure, Sequence
from salma.model.distributions import UniformDistribution,\
    Distribution
import math
from pprint import pprint

class ExogenousActionTest(unittest.TestCase):
    
    def setUp(self):
        self.__domains = dict()
        
        self.__domains['robot'] = [Agent('rob1', 'robot', Procedure(Sequence([]))),
                                   Agent('rob2', 'robot', Procedure(Sequence([]))),
                                   Agent('rob3', 'robot', Procedure(Sequence([])))
                                   ]
        
        self.__domains['meteor'] = [Entity('m1', 'meteor'), Entity('m2', 'meteor'), Entity('m3', 'meteor')]
        
        
        
    
    
    def getDomain(self, sortName):
        return self.__domains[sortName]
    
    def getFluentValue(self, fluentName, *fluentParams):
        pass
    
    
    
   
    def testCreateExogenousAction(self):
        
        class MyDist(Distribution):
            def __init__(self):
                Distribution.__init__(self, 'bool')
            
            def generateSample(self, domainMetaModel, stateContext, paramValues):
                rob = paramValues[0]
                meteor = paramValues[1]
                return rob.getId()[-1:] == meteor.getId()[-1:]
        
        act = ExogenousAction('impact', 
                               [('r','robot'), 
                                ('m','meteor')
                                ],
                               MyDist()
                               ,
                               [
                                ('velocity', UniformDistribution('float', (0,200.0) ) ),
                                ('angle', UniformDistribution('float', (0,math.pi)  ) )                                  
                                ]
                               )
        
   
        for r in self.getDomain('robot'):
            print(r)
        
        instances = act.generateInstances(self, self)
        pprint(instances)
        


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testCreateExogenousAction']
    unittest.main()