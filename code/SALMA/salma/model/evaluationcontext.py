from salma.SALMAException import SALMAException
from salma.model.infotransfer import Connector


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
        """
        :param EvaluationContext|None parent: the parent context
        """
        self.__sequenceIndexes = dict()
        self.__resultListIndexes = dict()
        self.__resultLists = dict()
        self.__parent = parent
        self.__procedureCall = None
        self.__agent = None
        self.__process = None
        self.__variable_bindings = dict()
        if self.__parent is None:
            self.__global_variables = dict()
        else:
            self.__global_variables = None

    def set_procedure_call(self, procedure_call):
        self.__procedureCall = procedure_call
        
    def get_procedure_call(self):
        """
        :rtype ProcedureCall
        """
        return self.__procedureCall
    
    def set_agent(self, agent):
        self.__agent = agent
        
    def get_agent(self):
        return self.__agent

    def get_parent(self):
        """
        :rtype EvaluationContext
        """
        return self.__parent

    def get_process(self):
        """
        :rtype: salma.model.process.Process
        """
        return self.__process

    def set_process(self, p):
        """
        :param salma.model.process.Process p: process
        """
        self.__process = p
    
    def create_child_context(self):
        """
        Creates a new evaluation context with the current one set as the parent.
        :rtype EvaluationContext
        """
        raise NotImplementedError()
    
    def evaluateCondition(self, sourceType, source, *params):
        """
        Evaluates the given condition.

        :param int sourceType: FLUENT, TRANSIENT_FLUENT, ECLP_FUNCTION or PYTHON_FUNCTION
        :param object source: a python function, the name of a fluent, or a pothon expression
        :param list params: parameters
        :returns: true if evaluation succeeded
        :rtype: bool
        """
        raise NotImplementedError()
    
    def evaluateFunction(self, sourceType, source, *params):
        # TODO: document
        raise NotImplementedError()

    def getFluentValue(self, fluentName, *params):
        """
        Returns the current value of the given fluent with the given parameters.
        If no value for the given fluent instance is defined, None is returned.

        :param str fluentName: the name of the fluent
        :param list params: the parameters qualifying the fluent instance
        :rtype: object
        """
        raise NotImplementedError()

    def get_current_time(self):
        """
        Returns the current world time.
        :rtype: int
        """
        raise NotImplementedError()

    def get_derived_fluent_value(self, fluent_name, params):
        """
        Returns the current value of the given derived fluent with the given parameters.

        :param str fluent_name: the name of the derived fluent.
        :param params:
        :rtype: object
        """
        raise NotImplementedError()

    def set_fluent_value(self, fluent_name, params, value):
        """
        Sets the current value of the given fluent instance.
        Note: this completely circumvents the progression mechanism and therefore falls out of the
         concurrency scheme. Therefore this should only be used for absolutely "isolated" fluents that only the current
         process can access.
        TODO: add automatic check
        :param fluent_name:
        :param params:
        :param value:
        :return:
        """
        raise NotImplementedError()

    def create_message(self, connector, agent, msg_type, params):
        """
        See documentation in Engine.create_message .

        :type connector: str
        :type agent: str
        :type msg_type: str
        :type params: list
        :rtype: int
        """
        raise NotImplementedError()

    def assignVariable(self, variableName, value):
        """
        Assigns the given value to the given variable name.

        :param str variableName: name of variable that should be set.
        :param object value: the value to assign
        """
        self.__variable_bindings[variableName] = value

    def assign_global_variable(self, var_name, value):
        if self.__parent is None:
            self.__global_variables[var_name] = value
        else:
            self.__parent.assignGlobalVariable(var_name, value)

    @property
    def variable_bindings(self):
        return self.__variable_bindings

    @property
    def global_variable_bindings(self):
        """
        Returns the global variable bindings of the root context.
        :rtype: dict[str, obj]
        """
        if self.__parent is None:
            return self.__global_variables
        else:
            return self.__parent.global_variable_bindings

    def resolve(self, *terms, **kwargs):
        """
        Evaluates each term in terms and returns a list with the collected results.
        """
        raise NotImplementedError()

    def determine_source_type(self, source, params):
        """
        Returns the source type as defined in EvaluationContext.
        :param object source: the source, either a Python function, a fluent name, or a Python expression.
        :param list|tuple params: the parameters used for evaluation.
        :rtype: int
        """
        raise NotImplementedError()

    def getEntity(self, entityId):
        """
        returns the entity with the given id
        """
        raise NotImplementedError()
    
    def getSorts(self):
        """
        Returns a list of all registered sorts. with all entities for the given sort.
        """
        raise NotImplementedError()
        
    def getDomain(self, sortName):
        """
        Returns a list with all entities for the given sort.
        :type sortName: str
        :rtype: list[Entity]
        """
        raise NotImplementedError()
    
    def getCurrentSequenceIndex(self, sequence):
        """
        Returns the current child index for the given procedure in this evaluation context.
        """
        if sequence in self.__sequenceIndexes:
            return self.__sequenceIndexes[sequence]
        else:
            return None
    
    def setCurrentSequenceIndex(self, sequence, index):
        self.__sequenceIndexes[sequence] = index
        
    def incCurrentSequenceIndex(self, sequence):
        if sequence in self.__sequenceIndexes:
            self.__sequenceIndexes[sequence] += 1
        else:
            raise(SALMAException("Trying to increment uninitialized sequence index."))

    def setCurrentResultListIndex(self, iteration, index):
        self.__resultListIndexes[iteration] = index
    
    def incCurrentResultListIndex(self, iteration):
        if iteration in self.__resultListIndexes:
            self.__resultListIndexes[iteration] += 1
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
        """
        Returns a list of dicts with {paramName => Entity} entries that fulfill the given predicate with
        the given parameters.

        The parameter list can include ground values, bound variables and (name, sort) tuples.
        """
        raise NotImplementedError()
    
    def selectFirst(self, predicateType, predicateName, *params):
        """
        Returns a dict with {paramName => Entity} entries that represents the first value combination
        that fulfills the given predicate with the given parameters.

        The parameter list can include ground values, bound variables and (name, sort) tuples.
        """
        raise NotImplementedError()
   
    def createPlan(self, procedureName, *params):
        raise NotImplementedError()
 
    def getActionClock(self, actionName, *params):
        """
        Returns the last recorded time when the given action was galled with the given parameters.

        If the given action-parameter combination hasn't occurred before, this method returns -1.

        :param str actionName: action name
        :param list params: parameters

        :rtype: int
        """
        raise NotImplementedError()
    
    def getFluentChangeTime(self, fluentName, *params):
        """
        Returns the last recorded time when the given fluent with the given parameters.

        If the given fluent-parameter combination has not been initialized yet,
        this method returns -1.

        :param fluentName: str
        :param params: list
        """
        raise NotImplementedError()
    
    def queryPersistentProperty(self, propertyName):
        """
        Returns a tuple with the current status of the given property together with the last
        change time.

        :param propertyName: str
        """
        raise NotImplementedError()

    def get_connector(self, name: str) -> Connector:
        """
        See documentation in World.get_connector
        """
        raise NotImplementedError()