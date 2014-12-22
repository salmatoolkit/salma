from salma.SALMAException import SALMAException
from salma.model.core import Entity
from salma.model.evaluationcontext import EvaluationContext
from salma.model.infotransfer import ReceivedMessage, Channel
from salma.constants import *


class Element(object):
    """
    The base class for all elements of a process, i.e. procedures and control elements.
    Each element keeps a unique, automatically generated id.
    """
    # static field
    __next_id = 1

    def __init__(self, element_id=None):
        """
        Creates a new element with the given id. If no id is given, a new value will be assigned by increasing
        Element.__next_id.
        """
        if element_id is None:
            self.__id = Element.__next_id
            Element.__next_id += 1
        else:
            self.__id = element_id

    def getId(self) -> int:
        return self.__id

    @property
    def id(self) -> int:
        """
        The element's id.
        """
        return self.__id


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

    def getParent(self):
        return self.__parent

    def setParent(self, parent):
        self.__parent = parent

    parent = property(getParent, setParent)

    def executeStep(self, evaluationContext, procedureRegistry):
        """
        Evalutes the control node, e.g. the test condition.

        Returns tuple: (CONTINUE|BLOCK, nextNode, nextEvaluationContext)
        """
        raise NotImplementedError()

    def reset(self, evaluationContext):
        raise NotImplementedError()

    # constants
    BLOCK = 0
    CONTINUE = 1


class Sequence(ControlNode):

    def __init__(self, new_children=None):
        if not new_children:
            new_children = []
        ControlNode.__init__(self)
        self.__children = []
        for c in new_children:
            self.addChild(c)

    def executeStep(self, evaluationContext, procedureRegistry):
        if evaluationContext.getCurrentSequenceIndex(self) is None:
            if len(self.__children) == 0:
                return ControlNode.CONTINUE, None, evaluationContext
            else:
                evaluationContext.setCurrentSequenceIndex(self, 0)

        csi = evaluationContext.getCurrentSequenceIndex(self)

        # check if the sequence has finished
        if csi > len(self.__children) - 1:
            evaluationContext.setCurrentSequenceIndex(self, 0)
            return ControlNode.CONTINUE, None, evaluationContext

        state, nextNode, nextContext = self.__children[csi].executeStep(evaluationContext, procedureRegistry)

        evaluationContext.incCurrentSequenceIndex(self)

        # if the child returned no next node, the sequence should handle the next step
        if nextNode is None:
            nextNode = self

        return state, nextNode, nextContext

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
    """
    A simple interface for keeping a registry for the defined procedures of an agent.
    """

    def __init__(self, registry=None):
        """
        param: an optional dictionary with name -> Procedure entries
        :param registry: dict
        """
        self.__registry = registry or dict()

    def getProcedure(self, procedureName):
        """
        Returns the procedure with the givent name.

        :param procedureName: str
        :rtype: Procedure
        """
        try:
            procedure = self.__registry[procedureName]
        except KeyError:
            raise SALMAException("Unregistered procedure: {}".format(procedureName))
        return procedure

    def registerProcedure(self, procedure):
        """
        Registers a procedure.

        :param Procedure procedure: the procedure to register
        """
        self.__registry[procedure.name] = procedure


class While(ControlNode):

    def __init__(self, condition_type, condition, condition_params, body):
        """
        Creates a new While node.

        :param int condition_type: EvaluationContext.FLUENT, ...
        :param condition: the condition
        :param list condition_params: the parameters that are used for evaluating the condition goal.
        :param ControlNode|list body: the loops body
        """
        ControlNode.__init__(self)
        self.__conditionType = condition_type
        self.__condition = condition
        self.__conditionGoalParams = condition_params
        self.__body = Sequence(body) if isinstance(body, list) else body
        self.__body.parent = self

    def getCondition(self):
        return self.__condition, self.__conditionGoalParams

    def getBody(self):
        """
        :rtype: ControlNode
        """
        return self.__body

    def executeStep(self, evaluationContext, procedureRegistry):
        result = evaluationContext.evaluateCondition(self.__conditionType,
                                                     self.__condition,
                                                     *self.__conditionGoalParams)

        if result is True:
            return ControlNode.CONTINUE, self.__body, evaluationContext
        else:
            return ControlNode.CONTINUE, None, evaluationContext

    def reset(self, evaluationContext):
        self.__body.reset(evaluationContext)


class If(ControlNode):
    """
    A typical IF-ElSE control structure.
    """

    def __init__(self, conditionType, condition, conditionGoalParams, thenBody, elseBody=None):
        """
        :param int conditionType: the condition type.
        :param object condition: the condition.
        :param tuple|list conditionGoalParams: the params for the condition.
        :param ControlNode|list thenBody: the THEN block
        :param ControlNode|list elseBody: the ELSE block
        """
        ControlNode.__init__(self)
        self.__conditionType = conditionType
        self.__condition = condition
        self.__conditionGoalParams = conditionGoalParams
        self.__thenBody = Sequence(thenBody) if isinstance(thenBody, list) else thenBody
        self.__elseBody = Sequence(elseBody) if isinstance(elseBody, list) else elseBody
        self.__thenBody.parent = self
        if self.__elseBody is not None:
            self.__elseBody.parent = self

    def getCondition(self):
        return self.__condition, self.__conditionGoalParams

    def getThenBody(self):
        return self.__thenBody

    def executeStep(self, evaluationContext, procedureRegistry):
        index = evaluationContext.getCurrentSequenceIndex(self) or 0
        # check if we're re-entering after the control bubbles up 
        # after execution of the then or else body
        if index > 0:
            self.reset(evaluationContext)
            return ControlNode.CONTINUE, None, evaluationContext

        result = evaluationContext.evaluateCondition(self.__conditionType,
                                                     self.__condition,
                                                     *self.__conditionGoalParams)
        evaluationContext.setCurrentSequenceIndex(self, 1)

        if result is True:
            return ControlNode.CONTINUE, self.__thenBody, evaluationContext
        else:
            return ControlNode.CONTINUE, self.__elseBody, evaluationContext

    def reset(self, evaluationContext):
        self.__thenBody.reset(evaluationContext)
        if self.__elseBody is not None:
            self.__elseBody.reset(evaluationContext)
        evaluationContext.setCurrentSequenceIndex(self, 0)


class Iterate(ControlNode):
    """
    An iteration construct that returns result tuples for one or several line variables that are bound in the
    body.

    The predicate parameter list can contain ground values, bound variables and new free variables as
        (name, type) tuples.
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
            return ControlNode.CONTINUE, None, evaluation_context

        valueCombination = resultList[resultIndex]
        evaluation_context.incCurrentResultListIndex(self)

        for varName, value in valueCombination.items():
            evaluation_context.assignVariable(varName, value)

        return ControlNode.CONTINUE, self.__body, evaluation_context

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

        return ControlNode.CONTINUE, None, evaluationContext

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
        return ControlNode.CONTINUE, None, evaluationContext


class Wait(ControlNode):
    """
    Blocks the process until the given condition is fulfilled.
    """

    def __init__(self, condition_type, condition, condition_goal_params):
        """
        :param int condition_type: the condition type
        :param object condition: the condition
        :param list condition_goal_params: parameters
        """
        ControlNode.__init__(self)
        self.__condition_type = condition_type
        self.__condition = condition
        self.__condition_goal_params = condition_goal_params

    def get_condition(self):
        """
        Returns the condition as a tuple (condition_type, condition, params)
        :rtype: (int, object, list)
        """
        return self.__condition_type, self.__condition, self.__condition_goal_params

    def executeStep(self, evaluation_context, procedure_registry):
        result = evaluation_context.evaluateCondition(self.__condition_type, self.__condition,
                                                      *self.__condition_goal_params)
        if result is True:
            return ControlNode.CONTINUE, None, evaluation_context
        else:
            return ControlNode.BLOCK, self, evaluation_context

    def reset(self, evaluation_context):
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
        return ControlNode.BLOCK, self, evaluationContext

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
        return ControlNode.CONTINUE, None, evaluationContext

    def reset(self, evaluationContext):
        pass


class SetFluent(ControlNode):
    def __init__(self, fluent_name: str, fluent_params: list, source_type: int, source, params: list):
        ControlNode.__init__(self)
        self.__fluent_name = fluent_name
        self.__fluent_params = fluent_params
        self.__source_type = source_type
        self.__source = source
        self.__params = params

    @property
    def fluent_name(self):
        return self.__fluent_name

    @property
    def fluent_params(self):
        return self.__fluent_params

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
        ground_source_params = evaluation_context.resolve(*self.__params)
        ground_fluent_params = evaluation_context.resolve(*self.__fluent_params)
        val = evaluation_context.evaluateFunction(self.__source_type, self.__source, *ground_source_params)
        evaluation_context.set_fluent_value(self.__fluent_name, ground_fluent_params, val)
        return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluationContext):
        pass


class ArbitraryAction(ControlNode):
    """
    An action that executes a python function.
    """

    def __init__(self, handler, *params):
        """
        Installs the given Python function as action handler.

        :param  handler: function supporting the given signature that returns a tuple of the form (stae, next_node) where
        state is either ControlNode.CONTINUE or ControlNode.BLOCK and next_node is a control node or None.
        """
        ControlNode.__init__(self)
        self.__handler = handler
        self.__params = params

    def executeStep(self, evaluation_context, procedure_registry):
        ground_params = evaluation_context.resolve(*self.__params)
        state, next_node = self.__handler(*ground_params)
        return state, next_node, evaluation_context

    def reset(self, evaluation_context):
        pass


class ProcedureCall(ControlNode):
    def __init__(self, procedure_name, procedure_parameters):
        """
        actionParameters can be terms, need metamodel for that. Terms have to be evaluated first to ground them.
        For now, variables should be enough...

        The agent will be inserted as an implicit 1st parameter.
        """
        ControlNode.__init__(self)
        self.__procedure_name = procedure_name
        self.__procedure_parameters = procedure_parameters

    @property
    def procedure_name(self):
        return self.__procedure_name

    @property
    def parameters(self):
        return tuple(self.__procedure_parameters)

    def executeStep(self, evaluation_context, procedure_registry):
        ground_params = evaluation_context.resolve(*self.__procedure_parameters)
        child_context = evaluation_context.createChildContext()

        child_context.setProcedureCall(self)
        procedure = procedure_registry.getProcedure(self.__procedure_name)

        if len(ground_params) != len(procedure.parameters):
            raise SALMAException(
                "Wrong number of parameters in call to procedure {}! Expected {} but was {}.".format(
                    self.__procedure_name,
                    len(procedure.parameters),
                    len(ground_params)
                )
            )

        for i, p in enumerate(procedure.parameters):
            var_name = p[0]
            # TODO: type checking!
            child_context.assignVariable(var_name, ground_params[i])

        # - return self as action
        return ControlNode.CONTINUE, procedure.body, child_context

    def __str__(self, *args, **kwargs):
        return "Act({},{})".format(self.__procedure_name, self.__procedure_parameters)

    def reset(self, evaluation_context):
        pass


class Send(ControlNode):
    """
    Sends a message on a channel to a specific agent.
    """

    def __init__(self, channel, message, own_role=None, destination=None, destination_role=None):
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

        if evaluation_context.getCurrentSequenceIndex(self) is None:
            evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)

        if csi == 0:
            channel, agent, own_role, dest, dest_role, message = evaluation_context.resolve(
                self.__channel, Entity.SELF, self.__own_role, self.__destination, self.__destination_role,
                self.__message)

            # local vars:
            #   msg_type: str
            #   params: list[object]
            connector = evaluation_context.get_connector(channel)
            if connector is None:
                raise SALMAException("Undefined connector: {}\n".format(channel))

            if isinstance(connector, Channel):
                if own_role == connector.role1[0]:
                    if dest_role is None:
                        dest_role = connector.role2[0]
                elif own_role == connector.role2[0]:
                    if dest_role is None:
                        dest_role = connector.role1[0]
                else:
                    raise SALMAException("Role {} is undefined for channel {}.".format(own_role, connector.name))

                if dest_role is None:
                    raise SALMAException(
                        "Using wrong destination role {} for channel {} with source role {}.".format(dest_role,
                                                                                                     connector.name,
                                                                                                     own_role))

                if dest is None:
                    # assume multicast
                    if connector.mode != Channel.MULTICAST:
                        raise SALMAException("No destination given for unicast channel {}.".format(connector.name))
                    msg_type = MSG_TYPE_MULTICAST_SRC
                    params = [own_role]
                else:
                    # we allow direct messages for unicast and multicast channels
                    msg_type = MSG_TYPE_UNICAST
                    params = [own_role, dest, dest_role]
            else:
                raise SALMAException("Unsupported connector type for Send of connector {}.".format(type(connector)))

            msgid = evaluation_context.create_message(channel, agent, msg_type, params)
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


class TransmitRemoteSensorReading(ControlNode):
    """
    Transmits the most recent sensor readings for the given remote sensor.

    The transmitted value is taken from the local sensor specified in the remote sensor declaration.
    If the local sensor requires parameters, these can be given as the last argument
    """

    def __init__(self, remote_sensor_name, sensor_params=None):
        """
        :param str remote_sensor_name: the name of the remote sensor
        :param list sensor_params: the parameters that will be used for local sensing
        """
        if not sensor_params:
            sensor_params = []
        super().__init__()
        self.__remote_sensor_name = remote_sensor_name
        self.__sensor_params = sensor_params

    def executeStep(self, evaluation_context: EvaluationContext, procedure_registry):
        if evaluation_context.getCurrentSequenceIndex(self) is None:
            evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)

        if csi == 0:
            agent, remote_sensor_name, sensor_params = evaluation_context.resolve(Entity.SELF,
                                                                                  self.__remote_sensor_name,
                                                                                  self.__sensor_params)
            msgid = evaluation_context.create_message(remote_sensor_name, agent, MSG_TYPE_REMOTE_SENSOR_SRC,
                                                      sensor_params)

            req_transfer_action = Act("requestTransfer", [msgid])
            req_transfer_action.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, req_transfer_action, evaluation_context
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
        super().__init__()
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
        super().__init__()
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

            msgid = evaluation_context.create_message(sensor, agent, MSG_TYPE_SENSOR, params)
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


class UpdateRemoteSensor(ControlNode):
    """
    Updates the remote sensor map at the receiving agent using the most recent messages from the information sources.
    Afterwards, these messages are removed from the incoming queue.
    """

    def __init__(self, remote_sensor_name):
        """
        :param str remote_sensor_name: the name of the remote sensor
        """
        super().__init__()
        self.__remote_sensor_name = remote_sensor_name

    def executeStep(self, evaluation_context: EvaluationContext, procedure_registry):
        if evaluation_context.getCurrentSequenceIndex(self) is None:
            evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)

        if csi == 0:
            agent, remote_sensor_name = evaluation_context.resolve(Entity.SELF, self.__remote_sensor_name)

            update_action = Act("update_remote_sensor", [Entity.SELF, remote_sensor_name])
            update_action.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, update_action, evaluation_context
        elif csi == 1:
            agent, remote_sensor_name = evaluation_context.resolve(Entity.SELF, self.__remote_sensor_name)
            clean_queue_action = Act("clean_queue", [agent, remote_sensor_name, remote_sensor_name])
            clean_queue_action.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, clean_queue_action, evaluation_context
        else:
            evaluation_context.setCurrentSequenceIndex(self, 0)
            return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluation_context):
        evaluation_context.setCurrentSequenceIndex(self, 0)

