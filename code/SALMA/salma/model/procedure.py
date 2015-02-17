from salma.SALMAException import SALMAException
from salma.model.core import Entity
from salma.model.evaluationcontext import EvaluationContext
from salma.model.infotransfer import ReceivedMessage, Channel
from salma.constants import *
import inspect


def is_control_node_sequence(thelist):
    if not isinstance(thelist, (list, tuple)):
        return False
    else:
        for e in thelist:
            if isinstance(e, ControlNode):
                return True
            else:
                return is_control_node_sequence(e)


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

    @property
    def id(self) -> int:
        """
        The element's id.
        """
        return self.__id


def makevars(*args):
    """
    Creates one or several variables.
    :rtype: list[Variable]
    """
    newvars = []
    for arg in args:
        if isinstance(arg, str):
            newvar = Variable(arg)
        elif isinstance(arg, (tuple, list)) and len(arg) == 2:
            newvar = Variable(arg[0], arg[1])
        else:
            raise SALMAException("Wrong formal of variable specification: {}".format(arg))
        newvars.append(newvar)
    return newvars


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

    def __init__(self, *args):
        """
        Creates a procedure with the given name, parameters, and body.

        :param str procedure_name:
        """
        Element.__init__(self)
        if len(args) == 1:
            self.__name = "main"
            self.__parameters = []
            body = args[0]
        elif len(args) == 2:
            self.__name = args[0]
            self.__parameters = []
            body = args[1]
        elif len(args) == 3:
            self.__name = args[0]
            self.__parameters = args[1]
            body = args[2]
        else:
            raise SALMAException("Wrong argument count in Procedure: expected 1, 2, or 3 arguments but "
                                 "got {} instead.".format(args))
        if isinstance(body, (list, tuple)):
            self.__body = Sequence(body)
        elif isinstance(body, ControlNode):
            self.__body = body
        else:
            raise SALMAException("Wrong type for body in Procedure: expected list or ControlNode but "
                                 "got {}".format(type(body)))

    @property
    def body(self):
        """
        The procedure's body.
        :rtype: ControlNode
        """
        return self.__body

    @property
    def name(self):
        """
        The procedure's name.
        :rtype: str
        """
        return self.__name

    @property
    def parameters(self):
        """
        The procedure's parameters, given as tuples of form (name, sort).

        :rtype: list[(str, str)]
        """
        return self.__parameters

    def restart(self, evaluation_context: EvaluationContext):
        self.__body.reset(evaluation_context)


class ControlNode(Element):
    """
    The base class for all procedure statements.
    """
    def __init__(self):
        """
        Default constructor.
        """
        Element.__init__(self)
        self.__parent = None

    @property
    def parent(self):
        """
        The parent of the control node, e.g. the enclosing Sequence.
        :rtype: ControlNode
        """
        return self.__parent

    @parent.setter
    def parent(self, parent):
        """
        :param ControlNode parent: the parent of the control node, e.g. the enclosing Sequence.
        """
        self.__parent = parent

    def execute_step(self, evaluation_context, procedure_registry):
        """
        Evalutes the control node, e.g. the test condition.

        Returns tuple: (CONTINUE|BLOCK, nextNode, nextEvaluationContext)
        """
        raise NotImplementedError()

    def reset(self, evaluation_context):
        raise NotImplementedError()

    # constants
    BLOCK = 0
    CONTINUE = 1


class Sequence(ControlNode):
    """
    A sequence of statements, i.e. ControlNodes.
    """

    def __init__(self, new_children=None):
        """
        Creates a new sequence that contains the given children.

        :param list[ControlNode]|tuple[ControlNode] new_children: the children that are added to the sequence.
        """
        super().__init__()
        if not new_children:
            new_children = []
        self.__children = []
        for c in new_children:
            self.add_child(c)

    def execute_step(self, evaluation_context, procedure_registry):
        if evaluation_context.getCurrentSequenceIndex(self) is None:
            if len(self.__children) == 0:
                return ControlNode.CONTINUE, None, evaluation_context
            else:
                evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)

        # check if the sequence has finished
        if csi > len(self.__children) - 1:
            evaluation_context.setCurrentSequenceIndex(self, 0)
            return ControlNode.CONTINUE, None, evaluation_context

        state, next_node, next_context = self.__children[csi].execute_step(evaluation_context, procedure_registry)

        evaluation_context.incCurrentSequenceIndex(self)

        # if the child returned no next node, the sequence should handle the next step
        if next_node is None:
            next_node = self

        return state, next_node, next_context

    @property
    def children(self):
        """
        The children of the sequence.
        :rtype: list[ControlNode]
        """
        return self.__children

    def add_child(self, child):
        """
        Adds a child control node to the sequence. Alternatively, a list can be given to add
        a nested sequence.

        :param ControlNode|list[ControlNode] child: the child to add
        """
        if isinstance(child, list):
            seq = Sequence(child)
            seq.parent = self
            self.__children.append(seq)
        else:
            self.__children.append(child)
            child.parent = self

    def reset(self, evaluation_context):
        evaluation_context.setCurrentSequenceIndex(self, 0)
        # : :type node: ControlNode
        for node in self.__children:
            node.reset(evaluation_context)


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

    def get_procedure(self, procedure_name):
        """
        Returns the procedure with the givent name.

        :param procedure_name: str
        :rtype: Procedure
        """
        try:
            procedure = self.__registry[procedure_name]
        except KeyError:
            raise SALMAException("Unregistered procedure: {}".format(procedure_name))
        return procedure

    def register_procedure(self, procedure):
        """
        Registers a procedure.

        :param Procedure procedure: the procedure to register
        """
        self.__registry[procedure.name] = procedure


class While(ControlNode):
    """
    A typical while control structure that repeats the given body control node as long as the given condition
    evaluates to True.
    """

    def __init__(self, condition, *args):
        """
        Creates a new While node.

        :param object condition: the condition
        :param args: 1.: (optional) the parameters that are used for evaluating the condition goal, 2.:
        the loops body (ControlNode or list).
        """
        ControlNode.__init__(self)

        self.__condition = condition
        if len(args) == 1:
            self.__condition_params = []
            body = args[0]
        elif len(args) == 2:
            self.__condition_params = args[0]
            body = args[1]
        else:
            raise SALMAException("Wrong number of arguments in While: expected 1 or 2 but "
                                 "got {}".format(args))
        self.__body = Sequence(body) if isinstance(body, (list, tuple)) else body
        self.__body.parent = self

    @property
    def condition(self):
        """
        The condition of this while block.
        :rtype: object
        """
        return self.__condition

    @property
    def condition_params(self):
        """
        The parameters of the condition.
        :rtype: list[(str, str)]
        """
        return self.__condition_params

    @property
    def body(self):
        """
        The body of this while block.
        :rtype: ControlNode
        """
        return self.__body

    def execute_step(self, evaluation_context, procedure_registry):
        condition_type = evaluation_context.determine_source_type(self.condition, self.condition_params)
        result = evaluation_context.evaluateCondition(condition_type,
                                                      self.condition,
                                                      *self.condition_params)

        if result is True:
            return ControlNode.CONTINUE, self.body, evaluation_context
        else:
            return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluation_context):
        self.body.reset(evaluation_context)


class If(ControlNode):
    """
    A typical IF-ElSE control structure.
    """

    def __init__(self, condition, *args):
        """
        Creates an IF-ELSE statement.

        :param object condition: the condition.
        :param args: 1. (optional): params, 2.: IF-body, 3. (optional): ELSE-body.
        """
        ControlNode.__init__(self)
        self.__condition = condition
        if len(args) == 1:
            self.__condition_params = []
            t_body = args[0]
            e_body = None
        elif len(args) == 2:
            if isinstance(args[0], ControlNode) or is_control_node_sequence(args[0]):
                t_body = args[0]
                e_body = args[1]
                self.__condition_params = []
            else:
                self.__condition_params = args[0]
                t_body = args[1]
                e_body = None
        elif len(args) == 3:
            self.__condition_params = args[0]
            t_body = args[1]
            e_body = args[2]
        else:
            raise SALMAException("Wrong number of arguments in IF: expected 1, 2, or 3 but got"
                                 "{}".format(args))
        self.__then_body = Sequence(t_body) if isinstance(t_body, list) else t_body
        self.__else_body = Sequence(e_body) if isinstance(e_body, list) else e_body
        self.__then_body.parent = self
        if self.__else_body is not None:
            self.__else_body.parent = self

    @property
    def condition(self):
        """
        The condition.
        :rtype: object
        """
        return self.__condition

    @property
    def condition_params(self):
        """
        The parameters of the condition.
        :rtype: list.
        """
        return self.__condition_params

    @property
    def then_body(self):
        """
        The control node that is executed if the condition evaluates to True.
        :rtype: ControlNode
        """
        return self.__then_body

    @property
    def else_body(self):
        """
        The control node that is executed if the condition evaluates to False.
        :rtype: ControlNode
        """
        return self.__else_body

    def execute_step(self, evaluation_context, procedure_registry):
        index = evaluation_context.getCurrentSequenceIndex(self) or 0
        # check if we're re-entering after the control bubbles up 
        # after execution of the then or else body
        if index > 0:
            self.reset(evaluation_context)
            return ControlNode.CONTINUE, None, evaluation_context
        condition_type = evaluation_context.determine_source_type(self.condition, self.condition_params)
        result = evaluation_context.evaluateCondition(condition_type,
                                                      self.condition,
                                                      *self.condition_params)
        evaluation_context.setCurrentSequenceIndex(self, 1)

        if result is True:
            return ControlNode.CONTINUE, self.then_body, evaluation_context
        else:
            return ControlNode.CONTINUE, self.else_body, evaluation_context

    def reset(self, evaluation_context):
        self.then_body.reset(evaluation_context)
        if self.else_body is not None:
            self.else_body.reset(evaluation_context)
        evaluation_context.setCurrentSequenceIndex(self, 0)


class Iterate(ControlNode):
    """
    An iteration construct that returns result tuples for one or several line variables that are bound in the
    body.

    The predicate parameter list can contain ground values, bound variables and new free variables as
        (name, type) tuples.
    The predicate is evaluated only once at the beginning for now.
    TODO: should we evaluate in each step?
    """

    def __init__(self, source, params, body):
        ControlNode.__init__(self)
        self.__source = source
        self.__params = params
        self.__body = Sequence(body) if isinstance(body, (list, tuple)) else body
        self.__body.parent = self

    @property
    def source(self):
        """
        The content source of the iteration.
        :rtype: object
        """
        return self.__source

    @property
    def params(self):
        """
        The i/out parameter list of the iteration.
        :rtype: list
        """
        return self.__params

    @property
    def body(self):
        """
        The iteration body that is executed in each iteration with the out-parameters bound to the
        values of the current row.
        :rtype: ControlNode
        """
        return self.__body

    def execute_step(self, evaluation_context, procedure_registry):
        """
        Executes a step of the iteration.

        :param EvaluationContext evaluation_context: the evaluation context
        :param ProcedureRegistry procedure_registry: the procedure registry
        """
        res_source = evaluation_context.resolve(self.source)
        res_params = evaluation_context.resolve(*self.params, strict=False)
        source_type = evaluation_context.determine_source_type(res_source, res_params)
        result_list = evaluation_context.getCurrentResultList(self)
        result_index = evaluation_context.getCurrentResultListIndex(self)

        # create result list only at the beginning of the iteration
        if result_list is None:
            result_list = evaluation_context.selectAll(
                source_type,
                self.source,
                *self.params)
            result_index = 0
            evaluation_context.setCurrentResultList(self, result_list)
            evaluation_context.setCurrentResultListIndex(self, result_index)

        # at the end of the iteration list, reset and pass control to parent
        if result_index > len(result_list) - 1:
            self.reset(evaluation_context)
            return ControlNode.CONTINUE, None, evaluation_context

        value_combination = result_list[result_index]
        evaluation_context.incCurrentResultListIndex(self)

        for varName, value in value_combination.items():
            evaluation_context.assignVariable(varName, value)

        return ControlNode.CONTINUE, self.body, evaluation_context

    def reset(self, evaluation_context):
        evaluation_context.setCurrentResultList(self, None)
        evaluation_context.setCurrentResultListIndex(self, -1)
        self.body.reset(evaluation_context)


class Select(ControlNode):
    """
    Selects the first value combination that makes predicateName with the given params true. The values are bound to
    variables with the given names in the current evaluation context.
    """

    def __init__(self, predicate, params):
        ControlNode.__init__(self)
        self.__predicate = predicate
        self.__params = params

    @property
    def predicate(self):
        return self.__predicate

    @property
    def params(self):
        """
        :rtype: list
        """
        return self.__params

    def execute_step(self, evaluation_context, procedure_registry):
        predicate_type = evaluation_context.determine_source_type(self.predicate, self.params)
        result = evaluation_context.selectFirst(predicate_type, self.predicate, *self.params)
        for var_name, value in result.items():
            evaluation_context.assignVariable(var_name, value)
        return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluation_context):
        pass


class Plan(ControlNode):
    def __init__(self, procedure_name, params, plan_name='plan'):
        ControlNode.__init__(self)
        self.__procedureName = procedure_name
        self.__params = params
        self.__planName = plan_name

    def reset(self, evaluation_context):
        pass

    @property
    def procedure_name(self):
        return self.__procedureName

    @property
    def parameters(self):
        return self.__params

    def execute_step(self, evaluation_context, procedure_registry):
        plan, values = evaluation_context.createPlan(self.procedure_name,
                                                     *self.parameters)

        for varName, value in values.items():
            evaluation_context.assignVariable(varName, value)

        evaluation_context.assignVariable('plan', plan)
        return ControlNode.CONTINUE, None, evaluation_context


class Wait(ControlNode):
    """
    Blocks the process until the given condition is fulfilled.
    """

    def __init__(self, condition, condition_params=None, **kwargs):
        """
        :param object condition: the condition
        :param list condition_params: parameters
        :param timeout: [obtional]
        """
        ControlNode.__init__(self)
        self.__condition = condition
        self.__condition_params = condition_params if condition_params is not None else []
        self.__timeout_expr = kwargs["timeout"] if "timeout" in kwargs else None
        self.__current_timeout = None

    @property
    def condition(self):
        return self.__condition

    @property
    def condition_params(self):
        """
        :rtype: list
        """
        return self.__condition_params

    @property
    def current_timeout(self):
        return self.__current_timeout

    def execute_step(self, evaluation_context, procedure_registry):
        self.__current_timeout = None
        condition_type = evaluation_context.determine_source_type(self.condition, self.condition_params)
        result = evaluation_context.evaluateCondition(condition_type, self.condition,
                                                      *self.condition_params)
        if result is True:
            return ControlNode.CONTINUE, None, evaluation_context
        else:
            if self.__timeout_expr is not None:
                current_time = evaluation_context.getFluentValue('time')
                self.__current_timeout = current_time + evaluation_context.resolve(self.__timeout_expr)[0]
            return ControlNode.BLOCK, self, evaluation_context

    def reset(self, evaluation_context):
        pass


class Act(ControlNode):
    """
    A control node that represents the execution of an action.
    """

    def __init__(self, action_name, action_parameters):
        """
        action_parameters can be terms, need metamodel for that. Terms have to be evaluated first to ground them.
        For now, variables should be enough...

        The agent will be inserted as an implicit 1st parameter.
        """
        ControlNode.__init__(self)
        self.__action_name = action_name
        self.__action_parameters = action_parameters

    @property
    def action_name(self):
        return self.__action_name

    @property
    def action_parameters(self):
        return tuple(self.__action_parameters)

    def execute_step(self, evaluation_context, procedure_registry):
        # - return self as action
        return ControlNode.BLOCK, self, evaluation_context

    def __str__(self, *args, **kwargs):
        return "Act({},{})".format(self.action_name, self.action_parameters)

    def reset(self, evaluation_context):
        pass


class Assign(ControlNode):
    """
    A statement that calculates a value and assigns it to the given variable.
    """
    def __init__(self, variables, source, params=None):
        """
        Creates an assign statement.

        :param Variable|str|list|tuple variables: the variable(s).
        :param object source: the source for the value calculation.
        :param list|tuple params:
        """
        super().__init__()
        newvars = []
        if isinstance(variables, (list, tuple)):
            newvars.extend(variables)
        else:
            newvars.append(variables)
        #: :type: list[str]
        self.__variable_names = []
        for v in newvars:
            if isinstance(v, Variable):
                self.__variable_names.append(v.name)
            else:
                self.__variable_names.append(str(v))

        self.__source = source
        self.__params = params if params is not None else []

    @property
    def variable_names(self):
        """
        :rtype: list[str]
        """
        return self.__variable_names

    @property
    def params(self):
        return self.__params

    @property
    def source(self):
        return self.__source

    def execute_step(self, evaluation_context, procedure_registry):
        source_type = evaluation_context.determine_source_type(self.source, self.params)
        ground_params = evaluation_context.resolve(*self.params)
        val = evaluation_context.evaluateFunction(source_type, self.source, *ground_params)
        if len(self.variable_names) == 1:
            evaluation_context.assignVariable(self.variable_names[0], val)
        else:
            for i in range(len(self.variable_names)):
                evaluation_context.assignVariable(self.variable_names[i], val[i])
        # TODO: proper error handling
        return ControlNode.CONTINUE, None, evaluation_context

    def reset(self, evaluation_context):
        pass


class FunctionControlNode(ControlNode):
    """
    An action that executes a python function.
    """

    def __init__(self, handler, *params):
        """
        Installs the given Python function as action handler.

        :param  handler: function supporting the given signature that returns a tuple of the form
        (state, next_node) where state is either ControlNode.CONTINUE or ControlNode.BLOCK and
        next_node is a ControlNode or None.
        """
        ControlNode.__init__(self)
        self.__handler = handler
        self.__params = params

    def execute_step(self, evaluation_context, procedure_registry):
        ground_params = evaluation_context.resolve(*self.__params)
        aspec = inspect.getfullargspec(self.__handler)
        kwargs = dict()
        if aspec.varkw is not None or "agent" in aspec.kwonlyargs:
            agent = evaluation_context.resolve(Entity.SELF)
            kwargs["agent"] = agent
        if aspec.varkw is not None or "evaluation_context" in aspec.kwonlyargs:
            kwargs["evaluation_context"] = evaluation_context

        if len(kwargs) > 0:
            res = self.__handler(*ground_params, **kwargs)
        else:
            res = self.__handler(*ground_params)

        if res is None:
            state = ControlNode.CONTINUE
            next_node = None
            ec = evaluation_context
        elif isinstance(res, (tuple, list)) and len(res) >= 2:
            state = res[0]
            next_node = res[1]
            if len(res) >= 3:
                ec = res[2]
            else:
                ec = evaluation_context
        else:
            raise SALMAException("Wrong return value of control node function: was {}, but expected either None or "
                                 "(state, next_node, [evaluation_context]).".format(res))
        return state, next_node, ec

    def reset(self, evaluation_context):
        pass


class ProcedureCall(ControlNode):
    def __init__(self, procedure_name, procedure_parameters):
        """
        action_parameters can be terms, need metamodel for that. Terms have to be evaluated first to ground them.
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

    def execute_step(self, evaluation_context, procedure_registry):
        ground_params = evaluation_context.resolve(*self.__procedure_parameters)
        child_context = evaluation_context.createChildContext()

        child_context.setProcedureCall(self)
        procedure = procedure_registry.get_procedure(self.__procedure_name)

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

    def execute_step(self, evaluation_context, procedure_registry):
        """
        :type evaluation_context: salma.model.evaluationcontext.EvaluationContext
        :type procedure_registry: ProcedureRegistry
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
            req_transfer = Act("requestTransfer", [msgid])
            req_transfer.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, req_transfer, evaluation_context
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

    def execute_step(self, evaluation_context: EvaluationContext, procedure_registry):
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

    def execute_step(self, evaluation_context, procedure_registry):
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

    def execute_step(self, evaluation_context, procedure_registry):
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

    def reset(self, evaluation_context):
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

    def execute_step(self, evaluation_context, procedure_registry):
        """
        :type evaluation_context: EvaluationContext
        """
        if evaluation_context.getCurrentSequenceIndex(self) is None:
            evaluation_context.setCurrentSequenceIndex(self, 0)

        csi = evaluation_context.getCurrentSequenceIndex(self)
        agent, sensor, params = evaluation_context.resolve(Entity.SELF, self.__sensor, self.__params)
        if csi == 0:

            msgid = evaluation_context.create_message(sensor, agent, MSG_TYPE_SENSOR, params)
            req_transfer = Act("requestTransfer", [msgid])
            req_transfer.parent = self
            evaluation_context.incCurrentSequenceIndex(self)
            return ControlNode.CONTINUE, req_transfer, evaluation_context
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

    def execute_step(self, evaluation_context: EvaluationContext, procedure_registry):
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

