import sys
import salma.model.process as process
from salma.SMCException import SMCException
from salma.model.distributions import Distribution, \
    ArgumentIdentityDistribution, UniformDistribution

from .procedure import ProcedureRegistry
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
    An agent is an active entity that executes one or several processes.
    """

    def __init__(self, entity_id, sort_name, processes=[], procedure_registry=None):
        """
        Creates an agent with the given id, sort and control procedure. Additionally,
        a procedure registry can be specified to allow procedure calls within the agent's control
        procedure.

        :type entity_id: str
        :type sort_name: str
        :type processes: list of process.Process
        :type procedure_registry: ProcedureRegistry
        """
        Entity.__init__(self, entity_id, sort_name)
        self.__processes = processes
        self.__evaluation_context = None
        self.__procedure_registry = procedure_registry or ProcedureRegistry()

    @property
    def evaluation_context(self):
        """
        The evaluation context used by this agent.
        :rtype: EvaluationContext
        """
        return self.__evaluation_context

    @evaluation_context.setter
    def evaluation_context(self, ctx):
        """
        :type ctx: EvaluationContext
        """
        self.__evaluation_context = ctx
        if ctx is not None:
            ctx.setAgent(self)
            ctx.setProcedureCall(None)

    @property
    def procedure_registry(self):
        """
        The agent's procedure registry.
        :rtype: ProcedureRegistry
        """
        return self.__procedure_registry

    @property
    def processes(self):
        """
        The agent's processes.
        :rtype: list of process.Process
        """
        return self.__processes

    def add_process(self, p):
        """
        Adds a process to the agent's registry.

        :type p: process.Process
        """
        self.__processes.append(p)

    def restart(self):
        """
        Stops all of the agent's processes.
        """
        for p in self.processes:
            p.stop()

    def is_finished(self):
        for p in self.processes:
            if not p.is_terminated():
                return False
        return True

    def update_schedule(self):
        """
        Updates the current schedule and returns the list of currently executed processes.
        :rtype: list of process.Process
        """
        if self.evaluation_context is None:
            raise SMCException("No evaluation context for agent " + self.id)

        if self.procedure_registry is None:
            raise SMCException("No procedure registry given for agent " + self.id)

        #: :type: list of process.Process
        running_processes = []

        for p in self.processes:
            if p.state == process.Process.IDLE:
                if p.should_start():
                    p.start()
            if p.is_scheduled():
                running_processes.append(p)

        return running_processes











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
            raise SMCException("Unknown parameter {} for action {}.".format(parameter_name, self.__name))

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