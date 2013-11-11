'''
Created on 21.05.2013

@author: kroiss
'''

import random
import sys

from salma.SMCException import SMCException
from salma.model.distributions import Distribution, \
    ArgumentIdentityDistribution, UniformDistribution

from .procedure import ControlNode, ActionExecution, ProcedureRegistry


class Entity(object):
    """
    Base class for all passive and active (agents) entities of the system. Stores an unique id and a sort name.
    Entities have to be added via World.add after loading the declaration but before calling World.initialize.
    """
    SELF = 'self'
    
    def __init__(self, entityId, sortName):
        self.__id = entityId
        self.__sortName = sortName

    def getId(self): return self.__id
      
    id = property(getId, "The entity's id.")
    
    def getSortName(self): return self.__sortName
    sortName = property(getSortName)
    
    def __repr__(self):
        return ':'.join((str(self.__id), self.__sortName))
    
    

class Agent(Entity):
    """
    An agent is an active entity that has a control procedure.
    """
    def __init__(self, entityId, sortName, controlProcedure, procedureRegistry = None):
        '''
        controlProcedure:procedure.Procedure
        '''
        Entity.__init__(self, entityId, sortName)
        self.__controlProcedure = controlProcedure
        self.__currentControlNode = controlProcedure.body
        self.__currentEvaluationContext = self.__initialEvaluationContext = None
        self.__procedureRegistry = procedureRegistry or ProcedureRegistry()
        self.__pendingAction = None
        
    def getEvaluationContext(self): 
        return self.__currentEvaluationContext
    
    def getProcedureRegistry(self):
        return self.__procedureRegistry
    
    def setProcedureRegistry(self, procedureRegistry):
        self.__procedureRegistry = procedureRegistry
    
    def setEvaluationContext(self, ctx): 
        self.__initialEvaluationContext = self.__currentEvaluationContext = ctx
        ctx.setAgent(self)
        ctx.setProcedureCall(None)
    
    def getPendingAction(self):
        return self.__pendingAction
    
    def setPendingAction(self, pendingAction):
        self.__pendingAction = pendingAction
    
    
    evaluationContext = property(getEvaluationContext, setEvaluationContext) 
    
    
    
    def restart(self):
        '''
        Sets the control pointer to the control procedure's body and restarts the procedure.
        '''
        self.__currentEvaluationContext = self.__initialEvaluationContext
        self.__currentControlNode = self.__controlProcedure.body
        self.__controlProcedure.restart(self.__currentEvaluationContext)
        
        
    def isFinished(self):
        return self.__currentControlNode is None
    
    
    
    def step(self, newStep = True):
        '''
        proceed the agent's control procedure until a blocking node (test) or action is encountered
        
        return: an action that the agent is about to execute or 
        None if either a blocking node was reached or the control procedure finished 
        
        '''
        if self.__currentEvaluationContext is None:
            raise SMCException("No evaluation context for agent " + self.id)
        
        if self.__procedureRegistry is None:
            raise SMCException("No procedure registry given for agen " + self.id)
        
        if self.__currentControlNode is None:
            return None
        
        if self.__pendingAction is not None and newStep == False:
            return None
        self.__pendingAction = None
                
        status = ControlNode.CONTINUE
        currentNode = self.__currentControlNode
        currentContext = self.__currentEvaluationContext
        while status == ControlNode.CONTINUE and currentNode != None:
            status, nextNode, nextContext = currentNode.executeStep(currentContext, self.__procedureRegistry)
            
            #: :type nextContext: EvaluationContext
            if nextNode is None:
                if currentNode.parent is None:
                    # this means we just left some procedure
                    if nextContext.getProcedureCall() is not None:
                        nextNode = nextContext.getProcedureCall().parent
                        nextContext = nextContext.getParent()
                        
                else:
                    nextNode = currentNode.parent
            
            currentNode = nextNode
            currentContext = nextContext
            
        self.__currentControlNode = currentNode
        self.__currentEvaluationContext = currentContext
        
        if isinstance(self.__currentControlNode, ActionExecution):
            action = self.__currentControlNode
            
            # set pointer to enclosing sequence if there is one
            if self.__currentControlNode.parent != None:
                self.__currentControlNode = self.__currentControlNode.parent 
            return action  # return action
        else:
            return None
        
               
class Fluent(object):
    
    DEFAULT_RANGE = (0, sys.maxsize)
    
    
    def __init__(self, name, fluentType, param_types, **kwargs):
        '''
        params contains pairs of name-sort of just sorts
        fluentType: either entity sort or integer, float or boolean
        '''
        
        self.__name = name
        self.__fluentType = fluentType
        self.__param_types = param_types
        
                
        if fluentType == "integer" or fluentType == "float":
            self.__valueRange = kwargs['range'] if 'range' in kwargs else Fluent.DEFAULT_RANGE
        else:
            self.__valueRange = None
        self.__distribution = (kwargs['distribution']
                               if 'distribution' in kwargs
                               else UniformDistribution(
                                    self.__fluentType, self.__valueRange))
        
        
        
        
    
    def setDistribution(self, distribution):
        self.__distribution = distribution
    
    def getDistribution(self): 
        return self.__distribution
    
    # def generateSample(self, fluentType, valueRange, domainMetaModel, paramValues):
        
    def generateSample(self, evaluationContext, paramValues): 
        return self.__distribution.generateSample(evaluationContext, paramValues)
        
        
    def getParameters(self):
        return self.__param_types
    

    def getName(self): return self.__name
    def getFluentType(self): return self.__fluentType
    
    name = property(getName)
    fluentType = property(getFluentType) 
    parameters = property(getParameters)
    
    def getValueRange(self): return self.__valueRange
    def setValueRange(self, valueRange): self.__valueRange = valueRange
    
    valueRange = property(getValueRange, setValueRange)
    



class Constant(Fluent):
    def __init__(self, name, fluentType, param_types, **kwargs):
        Fluent.__init__(self, name, fluentType, param_types, **kwargs)

    
    
class Action(object):
    
    def __init__(self, name, parameter_types, immediate=False):
        '''
        params: list of param sorts
        blocking: if true, the action's precondition (poss) will be evaluated before it is actually executed 
        '''
        self.__name = name
        self.__params = parameter_types
        self.__immediate = immediate
        
    @property
    def name(self):
        return self.__name
    
    @property
    def parameters(self):
        '''
        returns the parameter list as a list of tuples in the format (name, type)
        '''
        return self.__params

    @property
    def immediate(self):
        return self.__immediate

    @immediate.setter
    def immediate(self, is_immediate):
        self.__immediate = is_immediate