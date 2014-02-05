from salma.SALMAException import SALMAException


class Element(object):
    # static field
    __next_id = 1

    def __init__(self, elementId=None):
        if elementId is None:
            self.__id = Element.__next_id
            Element.__next_id += 1
        else:
            self.__id = elementId

    def getId(self):
        return self.__id

    id = property(getId, "The element's id.")


class Variable(object):
    def __init__(self, name, sort=None):
        self.__name = name
        self.__sort = sort

    def getName(self): return self.__name

    def getSort(self): return self.__sort

    name = property(getName)
    sort = property(getSort)


class Procedure(Element):
    def __init__(self, procedureName, parameters, body):
        Element.__init__(self)
        self.__body = body
        self.__name = procedureName
        self.__parameters = parameters


    @property
    def body(self):
        return self.__body

    @property
    def name(self):
        return self.__name

    @property
    def parameters(self):
        return self.__parameters

    def restart(self, evaluationContext):
        self.__body.reset(evaluationContext)


class ControlNode(Element):
    def __init__(self):
        Element.__init__(self)
        self.__parent = None


    def getParent(self): return self.__parent

    def setParent(self, parent): self.__parent = parent

    parent = property(getParent, setParent)


    def executeStep(self, evaluationContext, procedureRegistry):
        '''
        Evalutes the control node, e.g. the test condition. 
        
        Returns tuple: (CONTINUE|BLOCK, nextNode, nextEvaluationContext) 
        '''
        raise NotImplementedError()


    def reset(self, evaluationContext):
        raise NotImplementedError()

    # constants
    BLOCK = 0
    CONTINUE = 1


class Sequence(ControlNode):
    def __init__(self, newChildren=[]):
        ControlNode.__init__(self)
        self.__children = []
        for c in newChildren:
            self.addChild(c)


    def executeStep(self, evaluationContext, procedureRegistry):
        if evaluationContext.getCurrentSequenceIndex(self) is None:
            if len(self.__children) == 0:
                return (ControlNode.CONTINUE, None, evaluationContext)
            else:
                evaluationContext.setCurrentSequenceIndex(self, 0)

        csi = evaluationContext.getCurrentSequenceIndex(self)

        # check if the sequence has finished
        if csi > len(self.__children) - 1:
            evaluationContext.setCurrentSequenceIndex(self, 0)
            return (ControlNode.CONTINUE, None, evaluationContext)

        state, nextNode, nextContext = self.__children[csi].executeStep(evaluationContext, procedureRegistry)

        evaluationContext.incCurrentSequenceIndex(self)

        # if the child returned no next node, the sequence should handle the next step
        if nextNode is None:
            nextNode = self

        return (state, nextNode, nextContext)


    def getChildren(self):
        return self.__children

    def addChild(self, child):
        '''
        child:ControlNode
        '''
        self.__children.append(child)
        child.parent = self

    def reset(self, evaluationContext):
        evaluationContext.setCurrentSequenceIndex(self, 0)
        #: :type node: ControlNode
        for node in self.__children:
            node.reset(evaluationContext)


class ProcedureRegistry(object):
    '''
    A simple interface for keeping a registry for the defined procedures of an agent.
    '''

    def __init__(self, registry=None):
        '''
        param: an optional dictionary with name -> Procedure entries
        :param registry: dict
        '''
        self.__registry = registry or dict()

    def getProcedure(self, procedureName):
        '''
        Returns the procedure with the givent name.
        
        :param procedureName: str
        :rtype: Procedure
        '''
        procedure = None
        try:
            procedure = self.__registry[procedureName]
        except KeyError:
            raise SALMAException("Unregistered procedure: {}".format(procedureName))
        return procedure

    def registerProcedure(self, procedure):
        '''
        Registers a procedure.
        
        :param procedureName: str
        :param procedureName: Procedure
        '''
        self.__registry[procedure.name] = procedure


class While(ControlNode):
    def __init__(self, conditionType, condition, conditionGoalParams, body):
        '''
        
        :param body: body
        :param conditionType:
        :param condition:
        
        conditionType: name of the condition goal. The goal has to be defined in the domain module.
        conditionGoalParams: the parameters that are used for evaluating the condition goal.
        body:ControlNode
        conditionEvaluator:EvaluationContext
        '''
        ControlNode.__init__(self)
        self.__conditionType = conditionType
        self.__condition = condition
        self.__conditionGoalParams = conditionGoalParams
        self.__body = body
        self.__body.parent = self


    def getCondition(self):
        return (self.__condition, self.__conditionGoalParams)

    def getBody(self):
        '''
        :rtype ControlNode
        '''

        return self.__body

    def executeStep(self, evaluationContext, procedureRegistry):
        result = evaluationContext.evaluateCondition(self.__conditionType,
                                                     self.__condition,
                                                     *self.__conditionGoalParams)

        if result is True:
            return (ControlNode.CONTINUE, self.__body, evaluationContext)
        else:
            return (ControlNode.CONTINUE, None, evaluationContext)

    def reset(self, evaluationContext):
        self.__body.reset(evaluationContext)


class If(ControlNode):
    def __init__(self, conditionType, condition, conditionGoalParams, thenBody, elseBody):
        ControlNode.__init__(self)
        self.__conditionType = conditionType
        self.__condition = condition
        self.__conditionGoalParams = conditionGoalParams
        self.__thenBody = thenBody
        self.__elseBody = elseBody
        self.__thenBody.parent = self
        if not self.__elseBody is None:
            self.__elseBody.parent = self

    def getCondition(self):
        return (self.__condition, self.__conditionGoalParams)

    def getThenBody(self):
        return self.__thenBody

    def executeStep(self, evaluationContext, procedureRegistry):
        index = evaluationContext.getCurrentSequenceIndex(self) or 0
        # check if we're re-entering after the control bubbles up 
        # after execution of the then or else body
        if index > 0:
            self.reset(evaluationContext)
            return (ControlNode.CONTINUE, None, evaluationContext)

        result = evaluationContext.evaluateCondition(self.__conditionType,
                                                     self.__condition,
                                                     *self.__conditionGoalParams)
        evaluationContext.setCurrentSequenceIndex(self, 1)

        if result is True:
            return (ControlNode.CONTINUE, self.__thenBody, evaluationContext)
        else:
            return (ControlNode.CONTINUE, self.__elseBody, evaluationContext)

    def reset(self, evaluationContext):
        self.__thenBody.reset(evaluationContext)
        if self.__elseBody is not None:
            self.__elseBody.reset(evaluationContext)
        evaluationContext.setCurrentSequenceIndex(self, 0)


class Iterate(ControlNode):
    """
    The predicate's parameter list can contain ground values, bound variables and new free variables as (name, type) tuples.
    The predicate is evaluated only once at the beginning for now.
    TODO: should we evaluate in each step?
    """

    def __init__(self, source_type, source, params, body):
        ControlNode.__init__(self)
        self.__source_type = source_type
        self.__source = source
        self.__params = params
        self.__body = body
        self.__body.parent = self

    def executeStep(self, evaluation_context, procedure_registry):
        """
        Executes a step of the iteration.

        :param EvaluationContext evaluation_context: the evaluation context
        :param ProcedureRegistry procedure_registry: the procedure registry
        """

        resultList = evaluation_context.getCurrentResultList(self)
        resultIndex = evaluation_context.getCurrentResultListIndex(self)

        # create result list only at the beginning of the iteration
        if resultList is None:
            resultList = evaluation_context.selectAll(
                self.__source_type,
                self.__source,
                *self.__params)
            resultIndex = 0
            evaluation_context.setCurrentResultList(self, resultList)
            evaluation_context.setCurrentResultListIndex(self, resultIndex)


        # at the end of the iteration list, reset and pass control to parent
        if resultIndex > len(resultList) - 1:
            self.reset(evaluation_context)
            return (ControlNode.CONTINUE, None, evaluation_context)

        valueCombination = resultList[resultIndex]
        evaluation_context.incCurrentResultListIndex(self)

        for varName, value in valueCombination.items():
            evaluation_context.assignVariable(varName, value)

        return (ControlNode.CONTINUE, self.__body, evaluation_context)


    def reset(self, evaluationContext):
        evaluationContext.setCurrentResultList(self, None)
        evaluationContext.setCurrentResultListIndex(self, -1)

        self.__body.reset(evaluationContext)


class SelectFirst(ControlNode):
    """
    Selects the first value combination that makes predicateName with the given params true. The values are bound to
    variables with the given names in the current evaluation context.
    """

    def __init__(self, predicateType, predicateName, params):
        ControlNode.__init__(self)
        self.__predicateType = predicateType
        self.__predicateName = predicateName
        self.__params = params

    def executeStep(self, evaluationContext, procedureRegistry):
        result = evaluationContext.selectFirst(self.__predicateType, self.__predicateName, *self.__params)
        for varName, value in result.items():
            evaluationContext.assignVariable(varName, value)

        return (ControlNode.CONTINUE, None, evaluationContext)

    def reset(self, evaluationContext):
        pass


class Plan(ControlNode):
    def __init__(self, procedureName, params, planName='plan'):
        ControlNode.__init__(self)
        self.__procedureName = procedureName
        self.__params = params
        self.__planName = planName

    def reset(self, evaluationContext):
        pass


    @property
    def procedureName(self):
        return self.__procedureName

    @property
    def parameters(self):
        return self.__params


    def executeStep(self, evaluationContext, procedureRegistry):
        plan, values = evaluationContext.createPlan(self.__procedureName,
                                                    *self.__params)

        for varName, value in values.items():
            evaluationContext.assignVariable(varName, value)

        evaluationContext.assignVariable('plan', plan)
        return (ControlNode.CONTINUE, None, evaluationContext)


class Wait(ControlNode):
    def __init__(self, conditionType, condition, conditionGoalParams):
        ControlNode.__init__(self)
        self.__conditionType = conditionType
        self.__condition = condition
        self.__conditionGoalParams = conditionGoalParams

    def getCondition(self):
        return (self.__condition, self.__conditionGoalParams)

    def executeStep(self, evaluationContext, procedureRegistry):
        groundParams = evaluationContext.resolve(*self.__conditionGoalParams)

        result = evaluationContext.evaluateCondition(self.__conditionType, self.__condition, *groundParams)
        if result is True:
            return (ControlNode.CONTINUE, None, evaluationContext)
        else:
            return (ControlNode.BLOCK, self, evaluationContext)

    def reset(self, evaluationContext):
        pass


class ActionExecution(ControlNode):
    """
    A control node that represents the execution of an action.
    """

    def __init__(self, actionName, actionParameters):
        '''
        actionParameters can be terms, need metamodel for that. Terms have to be evaluated first to ground them.
        For now, variables should be enough...
        
        The agent will be inserted as an implicit 1st parameter. 
        '''
        ControlNode.__init__(self)
        self.__actionName = actionName
        self.__actionParameters = actionParameters

    def getActionName(self): return self.__actionName

    actionName = property(getActionName)

    def getActionParameters(self): return tuple(self.__actionParameters)

    actionParameters = property(getActionParameters)

    def executeStep(self, evaluationContext, procedureRegistry):
        # - return self as action
        return (ControlNode.BLOCK, self, evaluationContext)

    def __str__(self, *args, **kwargs):
        return "ActionExecution({},{})".format(self.__actionName, self.__actionParameters)

    def reset(self, evaluationContext):
        pass


class VariableAssignment(ControlNode):
    #TODO: handle assignment to multiple variables at once

    def __init__(self, variableName, sourceType, source, params):
        ControlNode.__init__(self)
        self.__variableName = variableName
        self.__sourceType = sourceType
        self.__source = source
        self.__params = params


    @property
    def variableName(self):
        return self.__variableName

    @property
    def sourceType(self):
        return self.__sourceType

    @property
    def params(self):
        return self.__params

    @property
    def source(self):
        return self.__source

    def executeStep(self, evaluationContext, procedureRegistry):
        groundParams = evaluationContext.resolve(*self.__params)
        val = evaluationContext.evaluateFunction(self.__sourceType, self.__source, *groundParams)
        evaluationContext.assignVariable(self.__variableName, val)
        return (ControlNode.CONTINUE, None, evaluationContext)

    def reset(self, evaluationContext):
        pass


class ArbitraryAction(ControlNode):
    '''
    An action thast executes a python function
    '''

    def __init__(self, handler, *params):
        '''
        handler: function supporting the given signature that returns either CONTINUE or BLOCK
        '''
        ControlNode.__init__(self)
        self.__handler = handler
        self.__params = params


    def executeStep(self, evaluationContext, procedureRegistry):
        groundParams = evaluationContext.resolve(*self.__params)
        state = self.__handler(*groundParams)
        return (state, None, evaluationContext)

    def reset(self, evaluationContext):
        pass


class ProcedureCall(ControlNode):
    def __init__(self, procedureName, procedureParameters):
        '''
        actionParameters can be terms, need metamodel for that. Terms have to be evaluated first to ground them.
        For now, variables should be enough...
        
        The agent will be inserted as an implicit 1st parameter. 
        '''
        ControlNode.__init__(self)
        self.__procedureName = procedureName
        self.__procedureParameters = procedureParameters


    @property
    def procedureName(self):
        return self.__actionName

    @property
    def parameters(self):
        return tuple(self.__actionParameters)


    def executeStep(self, evaluationContext, procedureRegistry):
        groundParams = evaluationContext.resolve(*self.__procedureParameters)
        childContext = evaluationContext.createChildContext()

        childContext.setProcedureCall(self)
        procedure = procedureRegistry.getProcedure(self.__procedureName)

        if len(groundParams) != len(procedure.parameters):
            raise SALMAException(
                "Wrong number of parameters in call to procedure {}! Expected {} but was {}.".format(
                    self.__procedureName,
                    len(procedure.parameters),
                    len(groundParams)
                )
            )

        for i, p in enumerate(procedure.parameters):
            varName = p[0]
            #TODO: type checking!
            childContext.assignVariable(varName, groundParams[i])




        # - return self as action
        return (ControlNode.CONTINUE, procedure.body, childContext)

    def __str__(self, *args, **kwargs):
        return "ActionExecution({},{})".format(self.__actionName, self.__actionParameters)

    def reset(self, evaluationContext):
        pass