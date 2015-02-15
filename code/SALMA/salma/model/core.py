from salma.SALMAException import SALMAException
from salma.model.distributions import Distribution, UniformDistribution
from .evaluationcontext import EvaluationContext


class Entity(object):
    """
    Base class for all passive and active (agents) entities of the system. Stores an unique id and a sort name.
    Entities have to be added via World.add after loading the declaration but before calling World.initialize.
    """
    SELF = 'self'

    def __init__(self, entity_id, sort_name):
        self.__id = entity_id
        self.__sortName = sort_name

    # TODO: et rid of deprecated access to get* -functions -> use properties
    def getId(self):
        return self.__id

    @property
    def id(self) -> int:
        """
        The entity's id.
        """
        return self.__id

    def getSortName(self):
        return self.__sortName

    @property
    def sortName(self) -> str:
        """
        The entity's sort name.
        """
        return self.__sortName

    def __repr__(self):
        return ':'.join((str(self.__id), self.__sortName))

    @property
    def evaluation_context(self):
        """
        The evaluation context used by this entity.
        :rtype: EvaluationContext
        """
        return None


class SituationDependentFunction(object):
    """
    Base class for fluents and derived fluents.
    """

    def __init__(self, name, fluent_type, params):
        """
        :param str name: the name.
        :param str fluent_type: the type of the value the fluent stores. This is boolean for relational fluents.
        :param list[(str, str)] params:  a list of (name, type) tuples for all of the fluent's parameters
        """
        self.__name = name
        self.__fluentType = fluent_type
        self.__params = params
        self.__param_indices = dict()
        for i, p in enumerate(params):
            self.__param_indices[p[0]] = i

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
        :raises SALMAException: if no parameter with the given name exists.
        :param parameter_name: the name of the parameter
        :type parameter_name: str
        :rtype: int
        """
        try:
            i = self.__param_indices[parameter_name]
            return i
        except KeyError:
            raise SALMAException("Unknown parameter {} for fluent {}.".format(parameter_name, self.__name))

    def get_parameter_type(self, parameter_name):
        """
        Returns the type of the parameter with the given name.
        :raises SALMAException: if no parameter with the given name exists.
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


class Fluent(SituationDependentFunction):
    """
    Represents a fluent.
    """
    DEFAULT_RANGE = (0, 2**31)

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
        super().__init__(name, fluent_type, params)

        if fluent_type == "integer" or fluent_type == "float":
            self.__valueRange = value_range
        else:
            self.__valueRange = None
        if fluent_type not in ["list", "term", "tuple"]:
            self.__distribution = (distribution if distribution is not None
                                   else UniformDistribution(self.fluentType, self.valueRange))
        else:
            self.__distribution = None

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

    def generateSample(self, evaluation_context, param_values):
        """
        Generates a sample for the fluent instance that is specified by paramValues.
        :type evaluation_context: EvaluationContext
        :type param_values: list|tuple
        :rtype: object
        """
        if self.__distribution is not None:
            return self.__distribution.generateSample(evaluation_context, param_values)
        else:
            return None

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

    def __str__(self):
        return "fluent: {}({}) -> {}".format(self.name, self.parameters, self.fluentType)


class Constant(Fluent):
    """
    Represents a constant, i.e. a special kind of fluent that is not changed in progression.
    """

    def __init__(self, name, fluent_type, params, value_range=Fluent.DEFAULT_RANGE, distribution=None):
        Fluent.__init__(self, name, fluent_type, params, value_range, distribution)


class DerivedFluent(SituationDependentFunction):

    def __init__(self, name, fluent_type, params):
        super().__init__(name, fluent_type, params)


class Action(object):
    """
    The base class for deterministic and stochastic action declarations. Stores name and parameter types.
    Each instances of Action represents the declaration of an action and is usually created and registered in
    World.load_declarations(). It it therefore uncommon to create Action instances manually.
    """

    def __init__(self, name, parameters):
        """
        :type name: str
        :type parameters: list[(str, str)]|tuple[(str,str)]
        """
        self.__name = name
        self.__params = parameters
        self.__param_indices = dict()
        for i, p in enumerate(parameters):
            self.__param_indices[p[0]] = i

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
        :raises SALMAException: if no parameter with the given name exists.
        :param parameter_name: the name of the parameter
        :type parameter_name: str
        :rtype: int
        """
        try:
            i = self.__param_indices[parameter_name]
            return i
        except KeyError:
            raise SALMAException("Unknown parameter {} for action {}.".format(parameter_name, self.__name))

    def get_parameter_type(self, parameter_name):
        """
        Returns the type of the parameter with the given name.
        :raises SALMAException: if no parameter with the given name exists.
        :type parameter_name: str
        :rtype: str
        """
        i = self.get_parameter_index(parameter_name)
        return self.__params[i][1]

    def describe(self):
        return "n/a"




