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
    
    
    def __init__(self, name, fluentType, params, **kwargs):
        '''
        params contains pairs of name-sort
        fluentType: either entity sort or int, float or bool
        '''
        
        self.__name = name
        self.__fluentType = fluentType
        self.__params = params
        
                
        if fluentType == "int" or fluentType == "float":
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
        return self.__params
    
    def getParamTypeByName(self, paramName):
        for param in self.__params:
            if param[0] == paramName:
                return param[1]
            
    def getName(self): return self.__name
    def getFluentType(self): return self.__fluentType
    
    name = property(getName)
    fluentType = property(getFluentType) 
    parameters = property(getParameters)
    
    def getValueRange(self): return self.__valueRange
    def setValueRange(self, valueRange): self.__valueRange = valueRange
    
    valueRange = property(getValueRange, setValueRange)
    



class Constant(Fluent):
    def __init__(self, name, fluentType, params, **kwargs):
        Fluent.__init__(self, name, fluentType, params, **kwargs)

    
    
class Action(object):
    
    def __init__(self, name, params, immediate=False):
        '''
        params: list of (name,sort) tuples
        blocking: if true, the action's precondition (poss) will be evaluated before it is actually executed 
        '''
        self.__name = name
        self.__params = params
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

    def getParamTypeByName(self, paramName):
        for param in self.__params:
            if param[0] == paramName:
                return param[1]
    
class DeterministicAction(Action):
    def __init__(self, name, params, immediate=False):
        Action.__init__(self, name, params, immediate)


        
        
class RandomActionOutcome(object):



    '''
    Determines the action and distributions for each parameter.
    
    The list of action parameters can contain Distribution instances or (name - type) tuples. The latter
    will be translated to instances of ArgumentIdentityDistribution with their current position in the argument
    list set as the ArgumentIdentityDistribution's argument position. For this reason, (name - type) tuples
    Have to be in the beginning of the parameter list.   
    '''
    def __init__(self, actionName, paramDistributionSpecs):
        '''

        :rtype : object
        actionName: the actionName
        paramDistributionSpecs: list of (paramName, ParamDistribution) tuples
        '''
        self.__actionName = actionName
        
        self.__paramDistributionSpecs = []
        for i, param in enumerate(paramDistributionSpecs):
            paramName = param[0]
            distr = (param[1] if isinstance(param[1], Distribution) 
                                else ArgumentIdentityDistribution(i))  
            self.__paramDistributionSpecs.append((paramName, distr))            
        
    @property
    def actionName(self):
        return self.__actionName
    
    @property
    def paramDistributionSpecs(self):
        return self.__paramDistributionSpecs
        
    def generateSample(self, evaluationContext, paramValues):
        '''
        generates a tuple with the form (actionName, params)
        '''
        args = []
        
        for pdistSpec in self.__paramDistributionSpecs:
            # pName = pdistSpec[0]
            pDistrib = pdistSpec[1]
            # generateSample(self, domainMetaModel, paramValues):
            val = pDistrib.generateSample(evaluationContext, paramValues)
            if isinstance(val, Entity):
                args.append(val.id)
            else:
                args.append(val)
                
        return (self.__actionName, args)

NOP_OUTCOME = RandomActionOutcome('nop', [])
class StochasticAction(Action):
    def __init__(self, name, params, outcomeSelector, immediate=False):
        '''
        :type name: str
        :type params: list
        :type outcomeSelector: function
        
        outcomeSelector signature: (domainMetaModel, stateContext, paramValues)
        '''
        Action.__init__(self, name, params, immediate)
        self.__outcomeSelector = outcomeSelector
        
    
    def generateOutcome(self, evaluationContext, paramValues):
        '''
        Returns a concrete sample as a (action, params) tuple.
        '''
        outcome = self.__outcomeSelector(evaluationContext, paramValues)
        return outcome.generateSample(evaluationContext, paramValues)
    
    
    
    
class ParametricStochasticAction(StochasticAction):
    '''
    A stochastic action that is parametric in the sense that there is only one outcome 
    specification.
    '''
    def __init__(self, actionName, paramDistributionSpecs, immediate=False):
        params = []
        for param in paramDistributionSpecs:
            paramName = param[0]
            paramSort = param[1].sort if isinstance(param[1], Distribution) else param[1]
            params.append((paramName, paramSort))
        StochasticAction.__init__(self, actionName, params, self.__selectOutcome, immediate)
        self.__outcome = RandomActionOutcome(actionName, paramDistributionSpecs)
  
    def __selectOutcome(self, evaluationContext, paramValues):
        return self.__outcome



class UniformStochasticAction(StochasticAction):
    '''
    stochastic 
    '''
    def __init__(self, name, params, outcomes, immediate=False):
        '''
        outcomes: list of RandomActionOutcome instances
        '''
        # TODO think about proper way to generalize
                
        StochasticAction.__init__(self, name, params, self.__selectOutcome, immediate)
        
        self.__outcomes = outcomes     
        
    
    def __selectOutcome(self, evaluationContext, paramValues):
        return random.choice(self.__outcomes)

            
class StepwiseStochasticAction(StochasticAction):
    '''
    stochastic 
    '''
    def __init__(self, name, params, outcomes, immediate=False):
        '''
        outcomes: list of (probability, RandomActionOutcome) tuples
        '''
        # TODO think about proper way to generalize
                
        StochasticAction.__init__(self, name, params, self.__selectOutcome, immediate)
        
        self.__outcomes = outcomes
        self.__validateDistribution()
        
        
        
    def __validateDistribution(self):
        s = sum(map(lambda x: x[0], self.__outcomes))
        if s != 1.0:
            raise(SMCException(
                    "Invalid probability distribution for stochastic action {}: probability sum was {} but should be 1.".format(
                                                                                                                        self.name, s)))
        
    
    def __selectOutcome(self, evaluationContext, paramValues):
        '''
        Selects the core.RandomActionOutcome that will then be used to generate the concrete sample.
        This is meant to be used as a template method and overridden in subclasses.
        '''
        r = random.uniform(0, 1)
        start = 0
        for spec in self.__outcomes:
            if (r >= start) and  (r < start + spec[0]):
                return spec[1]
            start += spec[0]
    
    


class ExogenousAction(object):
    
    # : :type __qualifyingParamDistributions: list
    __qualifyingParamDistributions = []
    
    def __init__(self, actionName, entityParams, occuranceDistribution, qualifyingParamDistributions):
        '''
        actionName: the actionName
        :type actionName: str
        
        entityParams: list of (paramName, sort) tuples
        :type entityParams: list
                
        occuranceDistribution: Distribution that receives a combination of entity values as parameters and decides whether the
            exogeneous action should occur for this combination
            
        :type occuranceDistribution: Distribution
        qualifyingParamDistributions: list of (paramName, ParamDistribution) tuples
        :type qualifyingParamDistributions: list
            
        '''
        self.__actionName = actionName
        
        
        self.__entityParams = entityParams
        
        self.__occuranceDistribution = occuranceDistribution           
            
        
        self.__qualifyingParamDistributions = qualifyingParamDistributions
        
        
    
    @property
    def actionName(self):
        return self.__actionName
    
    
    def shouldHappen(self, evaluationContext, paramValues):
        return self.__occuranceDistribution.generateSample(evaluationContext, paramValues)
        
    
    def generateInstance(self, evaluationContext, entityCombination):
        '''
        
        :param domainMetaModel: DomainMetaModel
        :param stateContext: StateContext
        '''
        args = list(entityCombination)
        
        
        # : :type distribution: Distribution
        for _, distribution in self.__qualifyingParamDistributions:
            val = distribution.generateSample(evaluationContext, args)
            args.append(val)
            
        refinedArgs = list(map(
                               lambda a: a.getId() if isinstance(a, Entity) else a, 
                               args)
                           )
                
        return (self.__actionName, refinedArgs)
       
        

        
    
        
