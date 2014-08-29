from salma.SALMAException import SALMAException
from salma.model.core import Entity
from salma.model.evaluationcontext import EvaluationContext
from salma.model.infotransfer import ReceivedMessage, Channel


class Element(object):
    """
    The base class for all elements of a process, i.e. procedures and control elements.
    Each element keeps a unique, automatically generated id.
    """
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
    """
    A class that marks a typed variable. This class is used in parameter lists of control nodes to
     distinguish it from other terms.
    """
    def __init__(self, name: str, sort: str=None):
        self.__name = name
        self.__sort = sort

    @property
    def name(self) -> str:
        return self.__name

    @property
    def sort(self) -> str:
        return self.__sort


class Procedure(Element):
    """
    A procedure that is used either as the main control flow of a process or as a sub-procedure.
    """

    def __init__(self, procedureName, parameters: list, body):
        """
        Creates a procedure with the given name, parameters, and body.
        """
        Element.__init__(self)
        self.__body = Sequence(body) if isinstance(body, list) else body
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

    def restart(self, evaluation_context: EvaluationContext):
        self.__body.reset(evaluation_context)


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
        """
        Adds a child control node to the sequence. Alternatively, a list can be given to create a sequence.

        :param ControlNode|list child: the child to add
        """
        if isinstance(child, list):
            seq = Sequence(child)
            seq.parent = self
            self.__children.append(seq)
        else:
            self.__children.append(child)
            child.parent = self

    def reset(self, evaluationContext):
        evaluationContext.setCurrentSequenceIndex(self, 0)
        # : :type node: ControlNode
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
        self.__body = Sequence(body) if isinstance(body, list) else body
        self.__body.parent = self


    def getCondition(self):
        return (self.__condition, self.__conditionGoalParams)

    def getBody(self):
        '''
        :rtype: ControlNode
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
    def __init__(self, conditionType, condition, conditionGoalParams, thenBody, elseBody=None):
        ControlNode.__init__(self)
        self.__conditionType = conditionType
        self.__condition = condition
        self.__conditionGoalParams = conditionGoalParams
        self.__thenBody = Sequence(thenBody) if isinstance(thenBody, list) else thenBody
        self.__elseBody = Sequence(elseBody) if isinstance(elseBody, list) else elseBody
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
        self.__body = Sequence(body) if isinstance(body, list) else body
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


class Select(ControlNode):
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


class Act(ControlNode):
    """
    A control node that represents the execution of an action.
    """

    def __init__(self, actionName, actionParameters):
        """
        actionParameters can be terms, need metamodel for that. Terms have to be evaluated first to ground them.
        For now, variables should be enough...

        The agent will be inserted as an implicit 1st parameter.
        """
        ControlNode.__init__(self)
        self.__actionName = actionName
        self.__actionParameters = actionParameters

    def getActionName(self):
        return self.__actionName

    actionName = property(getActionName)

    def getActionParameters(self):
        return tuple(self.__actionParameters)

    actionParameters = property(getActionParameters)

    def executeStep(self, evaluationContext, procedureRegistry):
        # - return self as action
        return (ControlNode.BLOCK, self, evaluationContext)

    def __str__(self, *args, **kwargs):
        return "Act({},{})".format(self.__actionName, self.__actionParameters)

    def reset(self, evaluationContext):
        pass


class Assign(ControlNode):
    # TODO: handle assignment to multiple variables at once

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


class SetFluent(ControlNode):
    def __init__(self, fluent_name, source_type, source, params):
        ControlNode.__init__(self)
        self.__fluent_name = fluent_name
        self.__source_type = source_type
        self.__source = source
        self.__params = params


    @property
    def fluent_name(self):
        return self.__fluent_name

    @property
    def source_type(self):
        return self.__source_type

    @property
    def params(self):
        return self.__params

    @property
    def source(self):
        return self.__source

    def executeStep(self, evaluation_context, procedure_registry):
        """
        :type evaluation_context: salma.model.evaluationcontext.EvaluationContext
        :param procedure_registry: ProcedureRegistry
        :rtype: (int, salma.model.evaluationcontext.EvaluationContext)
        """
        ground_params = evaluation_context.resolve(*self.__params)
        val = evaluation_context.evaluateFunction(self.__source_type, self.__source, *ground_params)
        evaluation_context.set_fluent_value(self.__fluent_name, ground_params, val)
        return ControlNode.CONTINUE, None, evaluation_context

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
            # TODO: type checking!
            childContext.assignVariable(varName, groundParams[i])




        # - return self as action
        return (ControlNode.CONTINUE, procedure.body, childContext)

    def __str__(self, *args, **kwargs):
        return "Act({},{})".format(self.__actionName, self.__actionParameters)

    def reset(self, evaluationContext):
        pass


class Send(ControlNode):
    """
    Sends a message on a channel to a specific agent.
    """

    def __init__(self, channel, own_role, message, destination=None, destination_role=None):
        ControlNode.__init__(self)
        self.__channel = channel
        self.__own_role = own_role
        self.__destination = destination

        self.__destination_role = destination_role
        self.__message = message

    def executeStep(self, evaluation_context, procedureRegistry):
        """
        :type evaluation_context: salma.model.evaluationcontext.EvaluationContext
        :type procedureRegistry: ProcedureRegistry
        :return:
        """
        msg_type = None
        connector = evaluation_context.get_connector(self.__channel)
        if connector is None:
            raise SALMAException("Undefined connector: %s\n".format(self.__channel))

        if isinstance(connector, Channel):
            connector.

        if evaluation_context.getCurrentSequenceIndex(self) is None:
            evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)

        if csi == 0:
            channel, agent, own_role, dest, dest_role, message = evaluation_context.resolve(
                self.__channel, Entity.SELF, self.__own_role, self.__destination, self.__destination_role,
                self.__message)

            msgid = evaluation_context.create_message(channel, agent, [own_role, dest, dest_role])
            evaluation_context.set_fluent_value("channel_out_content", [msgid], message)
            reqTransfer = Act("requestTransfer", [msgid])
            reqTransfer.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, reqTransfer, evaluation_context
        else:
            evaluation_context.setCurrentSequenceIndex(self, 0)
            return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluation_context):
        evaluation_context.setCurrentSequenceIndex(self, 0)


class Receive(ControlNode):
    def __init__(self, channel, role, variable):
        super().__init__()
        self.__channel = channel
        self.__role = role
        self.__variable = variable

    def executeStep(self, evaluation_context, procedureRegistry):
        if evaluation_context.getCurrentSequenceIndex(self) is None:
            evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)

        if csi == 0:
            agent, channel, role = evaluation_context.resolve(Entity.SELF, self.__channel, self.__role)
            queue_content = evaluation_context.evaluateFunction(EvaluationContext.TRANSIENT_FLUENT,
                                                                "local_channel_in_queue",
                                                                agent, channel, role)
            refined_queue_content = list()
            for msg in queue_content:
                refined_queue_content.append(ReceivedMessage(msg))

            evaluation_context.assignVariable(self.__variable, refined_queue_content)
            clean_queue = Act("clean_queue", [Entity.SELF, channel, role])
            clean_queue.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, clean_queue, evaluation_context
        else:
            evaluation_context.setCurrentSequenceIndex(self, 0)
            return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluation_context):
        evaluation_context.setCurrentSequenceIndex(self, 0)


class WaitForSensor(ControlNode):

    def __init__(self, sensor, agent, params, start_time):
        self.__sensor = sensor
        self.__agent = agent
        self.__params = params
        self.__start_time = start_time

    def executeStep(self, evaluation_context, procedureRegistry):
        """
        :type evaluation_context: EvaluationContext
        """

        sensor, agent, params, start_time = evaluation_context.resolve(self.__sensor, self.__agent, self.__params,
                                                                       self.__start_time)

        allparams = [agent] + params
        tstamp = evaluation_context.getFluentValue("tstamp_" + sensor, *allparams)
        if tstamp is None or tstamp < tstamp:
            return ControlNode.BLOCK, self, evaluation_context
        else:
            return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluationContext):
        pass


class Sense(ControlNode):
    """
    Performs asynchronous sensing on the given sensor with the given parameters. An optional variable
    can be specified in which case the sensing is synchronous and the result will be stored in this variable
    in addition to the local sensor fluent.
    """

    def __init__(self, sensor, params, variable=None):
        self.__sensor = sensor
        self.__params = params
        self.__variable = variable

    def executeStep(self, evaluation_context, procedureRegistry):
        """
        :type evaluation_context: EvaluationContext
        """
        if evaluation_context.getCurrentSequenceIndex(self) is None:
            evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)
        agent, sensor, params = evaluation_context.resolve(Entity.SELF, self.__sensor, self.__params)
        if csi == 0:

            msgid = evaluation_context.create_message(sensor, agent, params)
            reqTransfer = Act("requestTransfer", [msgid])
            reqTransfer.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, reqTransfer, evaluation_context
        elif csi == 1 and self.__variable is not None:
            # wait until transfer ends
            ctime = evaluation_context.getFluentValue("time")
            wait_for_sensor = WaitForSensor(sensor, agent, params, ctime)
            wait_for_sensor.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, wait_for_sensor, evaluation_context
        else:
            if self.__variable is not None:
                val = evaluation_context.getFluentValue(sensor, *params)
                evaluation_context.assignVariable(self.__variable, val)

            evaluation_context.setCurrentSequenceIndex(self, 0)
            return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluation_context):
        evaluation_context.setCurrentSequenceIndex(self, 0)
