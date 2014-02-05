from salma.SALMAException import SALMAException


class EvaluationContext(object):
    """
    An evaluation context provides access to fluents and variables. As variables are local to
    the currently running procedure, the common stack-based procedure call mechanism is used:
    - a fresh evaluation context has to be created for each procedure execution 
    - each evaluation context stores a reference to its parent
    - after a procedure is left, the parent is set as the current evaluation context
    - in order to find the correct point of return after a procedure is left, 
    the evaluation context stores also the current ProcedureCall control node. 
     
    """
    # source types
    FLUENT, TRANSIENT_FLUENT, ECLP_FUNCTION, PYTHON_FUNCTION, EXTENDED_PYTHON_FUNCTION, PYTHON_EXPRESSION,\
        CONSTANT, ITERATOR = range(8)

    def __init__(self, parent):
        '''
        :param parent: EvaluationContext
        '''
        self.__sequenceIndexes = dict()
        self.__resultListIndexes = dict()
        self.__resultLists = dict()
      
        
        self.__parent = parent
        self.__procedureCall = None
        self.__agent = None
    
    
    def setProcedureCall(self, procedureCall):
        self.__procedureCall = procedureCall
        
    def getProcedureCall(self):
        '''
        :rtype ProcedureCall
        '''
        return self.__procedureCall
    
    def setAgent(self, agent):
        self.__agent = agent
        
    def getAgent(self):
        return self.__agent
        
    
    def getParent(self):
        '''
        :rtype EvaluationContext
        ''' 
        return self.__parent
    
    def createChildContext(self):
        '''
        Creates a new evaluation context with the current one set as the parent.
        :rtype EvaluationContext
        '''
        raise NotImplementedError()
    
    def evaluateCondition(self, sourceType, source, *params):
        '''
        # TODO: document 
        sourceType: FLUENT, TRANSIENT_FLUENT, ECLP_FUNCTION or PYTHON_FUNCTION
        returns: true if evaluation succeeded
        '''
        raise NotImplementedError()
    
    def evaluateFunction(self, sourceType, source, *params):
        # TODO: document
        raise NotImplementedError()
        
    
    
    def getFluentValue(self, fluentName, *params):
        '''
        Returns the current value of the given fluent with the given parameters.
        
        :param fluentName: str
        :param params: list
        '''
        raise NotImplementedError()
    
    def assignVariable(self, variableName, value):
        '''
        Assigns the given value to the given variable name.
        
        variableName: name of the variable that should be set. 
        value: new value for variable
          
        '''
        raise NotImplementedError()
 
    def resolve(self, *terms):
        '''
        Evaluates each term in terms and returns a list with the collected results.
        '''        
        raise NotImplementedError()
    
    def getEntity(self, entityId):
        '''
        returns the entity with the given id
        '''
        raise NotImplementedError()
    
    def getSorts(self):
        '''
        Returns a list of all registered sorts. with all entities for the given sort.
        '''
        raise NotImplementedError()
        
    def getDomain(self, sortName):
        '''
        Returns a list with all entities for the given sort.
        '''
        raise NotImplementedError()
    
    def getCurrentSequenceIndex(self, sequence):
        '''
        Returns the current child index for the given procedure in this evaluation context. 
        '''
        if sequence in self.__sequenceIndexes:
            return self.__sequenceIndexes[sequence]
        else:
            return None
    
    def setCurrentSequenceIndex(self, sequence, index):
        self.__sequenceIndexes[sequence] = index
        
    def incCurrentSequenceIndex(self, sequence):
        if sequence in self.__sequenceIndexes:
            self.__sequenceIndexes[sequence] = self.__sequenceIndexes[sequence] + 1
        else:
            raise(SALMAException("Trying to increment uninitialized sequence index."))

    
    def setCurrentResultListIndex(self, iteration, index):
        self.__resultListIndexes[iteration] = index
    
    def incCurrentResultListIndex(self, iteration):
        if iteration in self.__resultListIndexes:
            self.__resultListIndexes[iteration] = self.__resultListIndexes[iteration] + 1
        else:
            raise(SALMAException("Trying to increment uninitialized result list index."))
    
           
    def getCurrentResultListIndex(self, iteration):
        if iteration in self.__resultListIndexes:
            return self.__resultListIndexes[iteration]
        else:
            return None
    
    def setCurrentResultList(self, iteration, resultList):
        self.__resultLists[iteration] = resultList
    
    def getCurrentResultList(self, iteration):
        if iteration in self.__resultLists:
            return self.__resultLists[iteration]
        else:
            return None

    def selectAll(self, predicateType, predicateName, *params):
        '''
        Returns a list of dicts with {paramName => Entity} entries that fulfill the given predicate with 
        the given parameters. 
        
        The parameter list can include ground values, bound variables and (name, sort) tuples.
        '''
        raise NotImplementedError()
    
    def selectFirst(self, predicateType, predicateName, *params):
        '''
        Returns a dict with {paramName => Entity} entries that represents the first value combination
        that fulfills the given predicate with the given parameters. 
        
        The parameter list can include ground values, bound variables and (name, sort) tuples.
        '''
        raise NotImplementedError()
   
    def createPlan(self, procedureName, *params):
        raise NotImplementedError()
 
 
    def getActionClock(self, actionName, *params):
        '''
        Returns the last recorded time when the given action was galled with the given parameters.
        
        If the given action-parameter combination hasn't occurred before, this method returns -1.
        
        :param actionName: str
        :param params: list  
        
        :rtype: int
        '''
        raise NotImplementedError()
    
    def getFluentChangeTime(self, fluentName, *params):
        '''
        Returns the last recorded time when the given fluent with the given parameters.
        
        If the given fluent-parameter combination has not been initialized yet, 
        this method returns -1.
        
        :param fluentName: str
        :param params: list
        '''
        raise NotImplementedError()
    
    def queryPersistentProperty(self, propertyName):
        '''
        Returns a tuple with the current status of the given property together with the last
        change time.
        
        :param propertyName: str
        '''
        raise NotImplementedError()
