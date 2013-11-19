'''
Created on 21.05.2013

@author: kroiss
'''

import random
import sys

from salma.SMCException import SMCException
from salma.model.distributions import Distribution, \
    ArgumentIdentityDistribution, UniformDistribution

from .procedure import ControlNode, ActionExecution, ProcedureRegistry, Procedure
from .evaluationcontext import EvaluationContext


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

    def __init__(self, entityId, sortName, controlProcedure, procedureRegistry=None):
        """
        Creates an agent with the given id, sort and control procedure. Adiitionally,
         a procedure registry can be specified to allow procedure calls within the agent's control
         procedure.

        :type entityId: str
        :type sortName: str
        :type controlProcedure: Procedure
        :type procedureRegistry: ProcedureRegistry
        """
        Entity.__init__(self, entityId, sortName)
        self.__controlProcedure = controlProcedure
        self.__currentControlNode = controlProcedure.body
        self.__currentEvaluationContext = self.__initialEvaluationContext = None
        self.__procedureRegistry = procedureRegistry or ProcedureRegistry()
        self.__pending_action = None

    @property
    def evaluation_context(self):
        """
        The evaluation context used by this agent.
        :rtype: EvaluationContext
        """
        return self.__currentEvaluationContext

    @evaluation_context.setter
    def evaluation_context(self, ctx):
        """
        :type ctx: EvaluationContext
        """
        self.__initialEvaluationContext = self.__currentEvaluationContext = ctx
        if ctx is not None:
            ctx.setAgent(self)
            ctx.setProcedureCall(None)

    @property
    def procedure_registry(self):
        return self.__procedureRegistry

    def get_pending_action(self):
        """
        Returns, if any, the action that was yielded by the agent during the immediate action gathering phase.
         This action is added to the non-immediate action list in World.step and will thus be executed after the
         immediate action phase.
        :rtype: (str, list)
        """
        return self.__pending_action

    def set_pending_action(self, pending_action):
        """
        :type pending_action: (str, list)
        """
        self.__pending_action = pending_action

    def restart(self):
        """
        Sets the control pointer to the control procedure's body and restarts the procedure.
        """
        self.__currentEvaluationContext = self.__initialEvaluationContext
        self.__currentControlNode = self.__controlProcedure.body
        self.__controlProcedure.restart(self.__currentEvaluationContext)

    def is_finished(self):
        """
        Returns True if the agent's control procedure has run to an end, i.e. the current control node is None.
        :rtype: bool
        """
        return self.__currentControlNode is None

    def step(self, new_step=True):
        """
        Proceed the agent's control procedure until a blocking node (test) or action is encountered.
        :param new_step: indicates that a new world step has just begun, i.e. we're in the first iteration through agents.
        :returns an action that the agent is about to execute or None if either a blocking node was reached or the
        control procedure finished
        :type new_step: bool
        :rtype: ActionExecution
        """
        if self.__currentEvaluationContext is None:
            raise SMCException("No evaluation context for agent " + self.id)

        if self.__procedureRegistry is None:
            raise SMCException("No procedure registry given for agen " + self.id)

        if self.__currentControlNode is None:
            return None

        if self.__pending_action is not None and new_step == False:
            return None
        self.__pending_action = None

        status = ControlNode.CONTINUE
        currentNode = self.__currentControlNode
        currentContext = self.__currentEvaluationContext
        while status == ControlNode.CONTINUE and currentNode is not None:
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
            if self.__currentControlNode.parent is not None:
                self.__currentControlNode = self.__currentControlNode.parent
            return action  # return action
        else:
            return None


class Fluent(object):
    """
    Represents a fluent.
    """
    DEFAULT_RANGE = (0, sys.maxsize)

    def __init__(self, name, fluent_type, params, value_range=DEFAULT_RANGE, distribution=None):
        """
        :param fluent_type: the type of the value the fluent stores. This is boolean for relational fluents.
        :param params:  a list of (name, type) tuples for all of the fluent's parameters

        :type name: str
        :type fluent_type: str
        :type params: list of (str, str)
        :type value_range: tuple
        :type distribution: Distribution
        """

        self.__name = name
        self.__fluentType = fluent_type
        self.__params = params
        self.__param_indices = dict()
        for i, p in enumerate(params):
            self.__param_indices[p[0]] = i

        if fluent_type == "integer" or fluent_type == "float":
            self.__valueRange = value_range
        else:
            self.__valueRange = None

        self.__distribution = (distribution if not distribution is None
                               else UniformDistribution(self.__fluentType, self.__valueRange))

    @property
    def distribution(self):
        """
        The distribution that is used to sample a value for a particular fluent instance.
        :rtype: Distribution
        """
        return self.__distribution

    @distribution.setter
    def distribution(self, distribution):
        """
        :type distribution: Distribution
        """
        self.__distribution = distribution

    def generateSample(self, evaluationContext, paramValues):
        """
        Generates a sample for the fluent instance that is specified by paramValues.
        :type evaluationContext: EvaluationContext
        :type paramValues: list
        :rtype: object
        """
        return self.__distribution.generateSample(evaluationContext, paramValues)

    @property
    def parameters(self):
        """
        The fluent's parameters as (name, type) tuples.
        :rtype: list of (str, str)
        """
        return self.__params

    def get_parameter_index(self, parameter_name):
        """
        Returns the index (i.e. position) of the parameter with the given name.
        :raises SMCException: if no parameter with the given name exists.
        :param parameter_name: the name of the parameter
        :type parameter_name: str
        :rtype: int
        """
        try:
            i = self.__param_indices[parameter_name]
            return i
        except KeyError:
            raise SMCException("Unknown parameter {} for fluent {}.".format(parameter_name, self.__name))

    def get_parameter_type(self, parameter_name):
        """
        Returns the type of the parameter with the given name.
        :raises SMCException: if no parameter with the given name exists.
        :type parameter_name: str
        :rtype: str
        """
        i = self.get_parameter_index(parameter_name)
        return self.__params[i][i]

    @property
    def name(self):
        """
        The fluent's name.
        :rtype: str
        """
        return self.__name

    @property
    def fluentType(self):
        """
        The fluent's type, i.e. integer, float, boolean or a user-defined sort.
        :rtype: str
        """
        return self.__fluentType

    @property
    def valueRange(self):
        """
        The possible value range for the fluent. This will be used for the default uniform
        sampling distribution.
        :rtype: tuple
        """
        return self.__valueRange

    @valueRange.setter
    def valueRange(self, valueRange):
        """
        :type valueRange: tuple
        """
        self.__valueRange = valueRange


class Constant(Fluent):
    """
    Represents a constant, i.e. a special kind of fluent that is not changed in progression.
    """

    def __init__(self, name, fluent_type, params, value_range=Fluent.DEFAULT_RANGE, distribution=None):
        Fluent.__init__(self, name, fluent_type, params, value_range, distribution)


class Action(object):
    """
    The base class for deterministic and stochastic action declarations. Stores name and parameter types.
    Each instances of Action represents the declaration of an action and is usually created and registered in
    World.load_declarations(). It it therefore uncommon to create Action instances manually.
    """

    def __init__(self, name, parameters, immediate=False):
        """
        :type name: str
        :type parameters: list of (str, str)
        :type immediate: bool
        TODO: blocking: if true, the action's precondition (poss) will be evaluated before it is actually executed
        """
        self.__name = name
        self.__params = parameters
        self.__param_indices = dict()
        for i, p in enumerate(parameters):
            self.__param_indices[p[0]] = i
        self.__immediate = immediate

    @property
    def name(self):
        """
        The action's name
        :rtype: str
        """
        return self.__name

    @property
    def parameters(self):
        """
        Returns the action's parameters as (name, type) tuples.
        :rtype: list of (str, str)
        """
        return self.__params

    def get_parameter_index(self, parameter_name):
        """
        Returns the index (i.e. position) of the parameter with the given name.
        :raises SMCException: if no parameter with the given name exists.
        :param parameter_name: the name of the parameter
        :type parameter_name: str
        :rtype: int
        """
        try:
            i = self.__param_indices[parameter_name]
            return i
        except KeyError:
            raise SMCException("Unknown parameter {} for fluent {}.".format(parameter_name, self.__name))

    def get_parameter_type(self, parameter_name):
        """
        Returns the type of the parameter with the given name.
        :raises SMCException: if no parameter with the given name exists.
        :type parameter_name: str
        :rtype: str
        """
        i = self.get_parameter_index(parameter_name)
        return self.__params[i][1]

    @property
    def immediate(self):
        """
        Whether the action is executed immediately, i.e. without blocking the agent for the rest of the current step.
        :rtype: bool
        """
        return self.__immediate

    @immediate.setter
    def immediate(self, is_immediate):
        """
        :type is_immediate: bool
        """
        self.__immediate = is_immediate