from salma.SALMAException import SALMAException
from salma.model.distributions import Distribution, UniformDistribution
from .evaluationcontext import EvaluationContext


def create_set_attr():
    def _sa(self, key, value):

        # object.__setattr__(self, key, value)
        print("sa", key, value)
        if self.__own_fluents is not None and key in self.__own_fluents:
            fl = self.__own_fluents[key]
            if len(fl.parameters) > 1:
                raise SALMAException("Trying to directly assign value to fluent instance {}.{} but need "
                                     "more parameters: {}".format(self.id, fl.name, fl.parameters))
            self.evaluation_context.set_fluent_value(fl.name, [self.id], value)
        elif self.__own_constants is not None and key in self.__own_constants:
            c = self.__own_constants[key]
            if len(c.parameters) > 1:
                raise SALMAException("Trying to directly assign value to constant {}.{} but need "
                                     "more parameters: {}".format(self.id, c.name, c.parameters))
            self.evaluation_context.set_constant_value(c.name, [self.id], value)
        elif self.__own_derived_fluents is not None and key in self.__own_derived_fluents:
            raise SALMAException("Trying to set value for derived fluent instance {}.{}".format(self.id, key))
        else:
            object.__setattr__(self, key, value)

    return _sa


class Entity(object):
    """
    Base class for all passive and active (agents) entities of the system. Stores an unique id and a sort name.
    Entities have to be added via World.add after loading the declaration but before calling World.initialize.
    """
    SELF = 'self'

    def __init__(self, entity_id, sort_name):
        self.__id = entity_id
        self.__sortName = sort_name
        self.__evaluation_context = None
        self.__own_fluents = dict()
        """:type : dict[str, Fluent]"""
        self.__own_derived_fluents = dict()
        """:type : dict[str, DerivedFluent]"""
        self.__own_constants = dict()
        """:type : dict[str, Constant]"""
        self.__initialized = True

    def __setattr__(self, key, value):
        if self.__initialized:
            if key in self.__own_fluents:
                self.__require_context()
                fl = self.__own_fluents[key]
                if len(fl.parameters) > 1:
                    raise SALMAException("Trying to directly assign value to fluent instance {}.{} but need "
                                         "more parameters: {}".format(self.id, fl.name, fl.parameters))
                pars = [] if len(fl.parameters) == 0 else [self.id]
                self.evaluation_context.set_fluent_value(fl.name, pars, value)
            elif key in self.__own_constants:
                self.__require_context()
                c = self.__own_constants[key]
                if len(c.parameters) > 1:
                    raise SALMAException("Trying to directly assign value to constant {}.{} but need "
                                         "more parameters: {}".format(self.id, c.name, c.parameters))
                pars = [] if len(c.parameters) == 0 else [self.id]
                self.evaluation_context.set_constant_value(c.name, pars, value)
            elif key in self.__own_derived_fluents:
                raise SALMAException("Trying to set value for derived fluent instance {}.{}".format(self.id, key))
            else:
                object.__setattr__(self, key, value)
        else:
            object.__setattr__(self, key, value)

    def __require_context(self):
        if self.evaluation_context is None:
            raise SALMAException(
                "No evaluation context defined for entity {} of sort {}.".format(self.id, self.sortName))

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
        return self.__evaluation_context

    @evaluation_context.setter
    def evaluation_context(self, ctx):
        """
        :type ctx: EvaluationContext
        """
        self.__evaluation_context = ctx
        if ctx is not None:
            self.initialize_evaluation_context(ctx)

    def initialize_evaluation_context(self, ctx: EvaluationContext):
        pass

    def get_own_fluent_value(self, fluent_name, *params):
        if self.evaluation_context is None:
            raise SALMAException("Calling get_own_fluent_value when no evaluation context is set.")

        return self.evaluation_context.get_fluent_value(fluent_name, *([self.id] + list(params)))

    def register_own_fluent(self, fluent):
        self.__own_fluents[fluent.name] = fluent

    def register_own_derived_fluent(self, derived_fluent):
        self.__own_derived_fluents[derived_fluent.name] = derived_fluent

    def register_own_constant(self, constant):
        self.__own_constants[constant.name] = constant

    def __handle_get_feature(self, item):

        if item in self.__own_fluents:
            fl = self.__own_fluents[item]
            if len(fl.parameters) == 1:
                return self.evaluation_context.get_fluent_value(fl.name, self.id)
            elif len(fl.parameters) == 0:
                return self.evaluation_context.get_fluent_value(fl.name)
            else:
                def __f(*params):
                    return self.evaluation_context.get_fluent_value(fl.name, *([self.id] + list(params)))

                return __f
        elif item in self.__own_derived_fluents:
            fl = self.__own_derived_fluents[item]
            if len(fl.parameters) == 1:
                return self.evaluation_context.get_derived_fluent_value(fl.name, [self.id])
            elif len(fl.parameters) == 0:
                return self.evaluation_context.get_derived_fluent_value(fl.name, [])
            else:
                def __f(*params):
                    return self.evaluation_context.get_derived_fluent_value(fl.name, [self.id] + list(params))

                return __f
        elif item in self.__own_constants:
            c = self.__own_constants[item]
            if len(c.parameters) == 1:
                return self.evaluation_context.get_constant_value(c.name, [self.id])
            elif len(c.parameters) == 0:
                return self.evaluation_context.get_constant_value(c.name, [])
            else:
                def __f(*params):
                    return self.evaluation_context.get_constant_value(fl.name, [self.id] + list(params))

                return __f
        else:
            raise SALMAException(
                "Feature {} not defined for entity {} of sort {}.".format(item, self.id, self.sortName))

    def __create_setter(self, fname):
        if fname in self.__own_fluents:
            fl = self.__own_fluents[fname]

            def __f(*params):
                if len(params) == 0:
                    raise SALMAException("No value specified when trying to set fluent {}.{}".format(self.id, fname))
                value = params[-1:][0]
                if len(fl.parameters) > 0:
                    pars = [self.id] + list(params[:-1])
                else:
                    pars = []
                self.evaluation_context.set_fluent_value(fname, pars, value)

            return __f
        elif fname in self.__own_constants:
            co = self.__own_constants[fname]

            def __f(*params):
                if len(params) == 0:
                    raise SALMAException("No value specified when trying to set constant {}.{}".format(self.id, fname))
                value = params[-1:][0]
                if len(co.parameters) > 0:
                    pars = [self.id] + list(params[:-1])
                else:
                    pars = []
                self.evaluation_context.set_constant_value(fname, pars, value)

            return __f
        elif fname in self.__own_derived_fluents:
            raise SALMAException("Trying to set value of derived fluent {}.{}".format(self.id, fname))
        else:
            raise SALMAException(
                "Feature {} not defined for entity {} of sort {}.".format(fname, self.id, self.sortName))

    def __getattr__(self, item):
        item = str(item)
        if item.endswith("__initialized"):
            return False
        if not self.__initialized:
            return object.__getattribute__(self, item)

        if item.startswith("set_"):
            feature_name = item[len("set_"):]
            return self.__create_setter(feature_name)
        else:
            return self.__handle_get_feature(item)

    def __eq__(self, other):
        if not isinstance(other, Entity):
            return NotImplemented
        return self.id == other.id and self.sortName == other.sortName

    def __lt__(self, other):
        if not isinstance(other, Entity):
            return NotImplemented
        return (self.sortName, self.id) < (other.sortName, other.id)

    def __gt__(self, other):
        if not isinstance(other, Entity):
            return NotImplemented
        return (self.sortName, self.id) > (other.sortName, other.id)

    def __le__(self, other):
        if not isinstance(other, Entity):
            return NotImplemented
        return not self.__gt__(other)

    def __ne__(self, other):
        if not isinstance(other, Entity):
            return NotImplemented
        return not self.__eq__(other)

    def __ge__(self, other):
        if not isinstance(other, Entity):
            return NotImplemented
        return not self.__lt__(other)

    def __hash__(self):
        return hash((self.sortName, self.id))


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
    DEFAULT_RANGE = (0, 2 ** 31)

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


class FluentValue(object):
    """
    Stores the value of a fluent instance.
    Note that no type is stored here.
    """

    def __init__(self, fluent_name, param_values, value):
        """
        :param str fluent_name: fluent name
        :param list param_values: list of params
        :param obj value: fluent value
        """
        self.__fluent_name = fluent_name
        self.__param_values = param_values
        self.__value = value

    @property
    def name(self):
        return self.__fluent_name

    @property
    def params(self):
        return self.__param_values

    @property
    def value(self):
        return self.__value

    def __str__(self):
        s = self.__fluent_name
        if len(self.__param_values) > 0:
            s += "("
            for p in self.__param_values[:-1]:
                s += p + ", "
            s += str(self.__param_values[-1]) + ")"

        s += " = " + str(self.__value)
        return s


class ConstantValue(FluentValue):
    def __init__(self, constant_name, params, value):
        super().__init__(constant_name, params, value)


def translate_entities(params):
    """
    Translates every entity entry to its id. Every other value will be passed trough.
    :param list params: the params
    :rtype: list
    """
    translated = []
    for p in params:
        if isinstance(p, Entity):
            translated.append(p.id)
        else:
            translated.append(p)
    return translated