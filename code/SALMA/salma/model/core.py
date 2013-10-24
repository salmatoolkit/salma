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


class DeterministicAction(Action):
    def __init__(self, name, parameter_types, immediate=False):
        Action.__init__(self, name, parameter_types, immediate)


        
        
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
        :type actionName: str
        :type paramDistributionSpecs: list

        actionName: the actionName
        paramDistributionSpecs: list of ParamDistribution objects

        '''
        self.__actionName = actionName
        
        self.__paramDistributionSpecs = []
        for i, param in enumerate(paramDistributionSpecs):
            distr = (param if isinstance(param, Distribution)
                                else ArgumentIdentityDistribution(i))  
            self.__paramDistributionSpecs.append(distr)
        
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
        
        for pdistrib in self.__paramDistributionSpecs:
            # generateSample(self, domainMetaModel, paramValues):
            val = pdistrib.generateSample(evaluationContext, paramValues)
            if isinstance(val, Entity):
                args.append(val.id)
            else:
                args.append(val)
                
        return (self.__actionName, args)

NOP_OUTCOME = RandomActionOutcome('nop', [])

class StochasticAction(Action):
    def __init__(self, name, parameter_types, outcomeSelector = None, immediate=False):
        '''
        :type name: str
        :type parameter_types: list
        :type outcomeSelector: function
        
        outcomeSelector signature: (domainMetaModel, stateContext, paramValues)
        '''
        Action.__init__(self, name, parameter_types, immediate)
        self.__outcomeSelector = outcomeSelector

    def getOutcomeSelector(self):
        return self.__outcomeSelector

    def setOutcomeSelector(self, selector):
        self.__outcomeSelector = selector
    
    def generateOutcome(self, evaluationContext, paramValues):
        '''
        Returns a concrete sample as a (action, params) tuple.
        '''

        outcome = self.__outcomeSelector(evaluationContext, paramValues)
        return outcome.generateSample(evaluationContext, paramValues)

def parametric_stochastic_action(actionName, paramDistributionSpecs):
    '''
    Creates a stochastic action outcome selector that is parametric in the sense that there is only one outcome
    specification.
    '''
    params = []
    for param in paramDistributionSpecs:
        paramSort = param.sort if isinstance(param, Distribution) else param
        params.append(paramSort)
    __outcome = RandomActionOutcome(actionName, paramDistributionSpecs)
    def __selectOutcome(evaluationContext, paramValues):
        return __outcome
    return __selectOutcome

def uniform_stochastic_action(outcomes):
    '''
    outcomes: list of RandomActionOutcome instances
    :type outcomes: list
    '''
    __outcomes = outcomes

    def __selectOutcome(evaluationContext, paramValues):
        return random.choice(__outcomes)

    return __selectOutcome
            
def stepwise_stochastic_action(outcomes_with_probabilities):
    '''
    outcomes_with_probabilities: list of (probability, RandomActionOutcome) tuples
    '''

    __outcomes_with_probabilities = outcomes_with_probabilities
    s = sum(map(lambda x: x[0], __outcomes_with_probabilities))
    if s != 1.0:
        raise(SMCException(
                "Invalid probability distribution: probability sum was {} but should be 1.".format(s)))

    def __selectOutcome(evaluationContext, paramValues):
        r = random.uniform(0, 1)
        start = 0
        for spec in __outcomes_with_probabilities:
            if (r >= start) and  (r < start + spec[0]):
                return spec[1]
            start += spec[0]
        return None
    return __selectOutcome



class ExogenousAction(object):
    
    # : :type __qualifyingParamDistributions: list
    __qualifyingParamDistributions = []
    
    def __init__(self, actionName, entity_param_types, qualifying_param_types, occuranceDistribution = None,
                 qualifyingParamDistributions = []):
        '''
        actionName: the actionName
        :type actionName: str
        
        entityParams: list of (paramName, sort) tuples
        :type entity_param_types: list
                
        occuranceDistribution: Distribution that receives a combination of entity values as parameters and decides whether the
            exogeneous action should occur for this combination
            
        :type occuranceDistribution: Distribution
        qualifyingParamDistributions: list of ParamDistribution
        :type qualifyingParamDistributions: list
            
        '''
        self.__actionName = actionName
        
        self.__entity_param_types = entity_param_types
        self.__qualifying_param_types = qualifying_param_types
        self.__occuranceDistribution = occuranceDistribution
        
        self.__qualifyingParamDistributions = qualifyingParamDistributions
        
        
    
    @property
    def actionName(self):
        return self.__actionName

    @property
    def entity_param_types(self):
        return self.__entity_param_types

    def get_occurance_distribution(self):
        return self.__occuranceDistribution

    def set_occurance_distribution(self, distrib):
        '''
        :type distrib: Distribution
        '''
        self.__occuranceDistribution = distrib

    def get_qualifying_param_distributions(self):
        return self.__qualifyingParamDistributions

    def set_qualifying_param_distributions(self, distribs):
        '''
        :type distribs: list
        '''
        self.__qualifyingParamDistributions = distribs
    
    def shouldHappen(self, evaluationContext, paramValues):
        return self.__occuranceDistribution.generateSample(evaluationContext, paramValues)
        
    
    def generateInstance(self, evaluationContext, entityCombination):
        '''
        
        :param domainMetaModel: DomainMetaModel
        :param stateContext: StateContext
        '''
        args = list(entityCombination)
        
        
        # : :type distribution: Distribution
        for distribution in self.__qualifyingParamDistributions:
            val = distribution.generateSample(evaluationContext, args)
            args.append(val)
            
        refinedArgs = list(map(
                               lambda a: a.getId() if isinstance(a, Entity) else a, 
                               args)
                           )
                
        return (self.__actionName, refinedArgs)
       
        

        
    
        
