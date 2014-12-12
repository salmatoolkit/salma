from salma.SALMAException import SALMAException
from salma.model.core import Entity
from salma.model.distributions import BernoulliDistribution, UniformDistribution, Distribution, NormalDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.termutils import tuplify
import numbers
import random


class ExogenousAction(object):
    """
    Represents an exogenous action, i.e. an event from the environment.
    """

    def __init__(self, action_name, entity_params, stochastic_params, configuration=None):
        """
        :param action_name: the action_name
        :param entity_params and stochastic_params: list of (param-name, sort) tuples

        :type action_name: str
        :type entity_params: list[(str, str)]
        :type stochastic_params: list[(str, str)]
        """
        self.__action_name = action_name
        self.__entity_params = entity_params
        # : :type : dict of (str, int)
        self.__entity_param_indices = dict()
        for i, p in enumerate(entity_params):
            self.__entity_param_indices[p[0]] = i

        self.__stochastic_params = stochastic_params
        # : :type : dict of (str, int)
        self.__stochastic_param_indices = dict()
        for i, p in enumerate(stochastic_params):
            self.__stochastic_param_indices[p[0]] = i

        self.__configuration = configuration if configuration is not None else ExogenousActionConfiguration(self)

    @property
    def action_name(self):
        """
        The exogenous action's name.
        :rtype: str
        """
        return self.__action_name

    @property
    def entity_params(self):
        """
        The entity parameters of this exogenous action, i.e. the parameters that constitute the exogenous
        action instance.
        :returns: a list of (name, type) tuples
        :rtype: list of (str, str)
        """
        return self.__entity_params

    @property
    def stochastic_params(self):
        """
        The stochastic parameters of the exogenous action that will be sampled according to the parameter
        distributions set up in the exogenous action's config.
        :return: a list of (name, type) tuples
        :rtype: list of (str, str)
        """
        return self.__stochastic_params

    def get_entity_parameter_index(self, parameter_name):
        """
        Returns the index (i.e. position) of the parameter with the given name.
        :raises SALMAException: if no parameter with the given name exists.
        :param parameter_name: the name of the parameter
        :type parameter_name: str
        :rtype: int
        """
        try:
            i = self.__entity_param_indices[parameter_name]
            return i
        except KeyError:
            raise SALMAException(
                "Unknown entity parameter {} for exogenous action {}.".format(parameter_name, self.__action_name))

    def get_entity_parameter_type(self, parameter_name):
        """
        Returns the type of the parameter with the given name.
        :raises SALMAException: if no parameter with the given name exists.
        :type parameter_name: str
        :rtype: str
        """
        i = self.get_entity_parameter_index(parameter_name)
        return self.__entity_params[i][1]

    def get_stochastic_parameter_index(self, parameter_name):
        """
        Returns the index (i.e. position) of the parameter with the given name.
        :raises SALMAException: if no parameter with the given name exists.
        :param parameter_name: the name of the parameter
        :type parameter_name: str
        :rtype: int
        """
        try:
            i = self.__stochastic_param_indices[parameter_name]
            return i
        except KeyError:
            raise SALMAException(
                "Unknown stochastic parameter {} for exogenous action {}.".format(parameter_name, self.__action_name))

    def get_stochastic_parameter_type(self, parameter_name):
        """
        Returns the type of the parameter with the given name.
        :raises SALMAException: if no parameter with the given name exists.
        :type parameter_name: str
        :rtype: str
        """
        i = self.get_stochastic_parameter_index(parameter_name)
        return self.__stochastic_params[i][1]

    @property
    def config(self):
        """
        The configuration of this exogenous action.
        :rtype: ExogenousActionConfiguration
        """
        return self.__configuration

    @config.setter
    def config(self, config):
        """
        :type config: ExogenousActionConfiguration
        """
        self.__configuration = config

    def should_happen(self, evaluation_context, param_values):
        """
        Uses the defined occurrence distribution to decide whether the event instance with the given parameters
        should be executed at the current time step.

        :param EvaluationContext evaluation_context: the evaluation context that is used by the
            occurrence distribution.
        :param list|tuple param_values: the parameters that are used for sampling from the occurrence distribution.
        :return: True if the event instance should be executed at the current time step.
        :rtype: bool
        """
        res = self.config.occurrence_distribution.generateSample(evaluation_context, param_values)
        assert isinstance(res, bool)
        return res

    def get_next_occurrence_time(self, evaluation_context, param_values):
        """
        Uses the defined occurrence distribution to determine the next time step when the
        event instance with the given parameters should be executed. This method may also return None
        which means that the event is not scheduled at all at this point.

        :param EvaluationContext evaluation_context: the evaluation context that is used by the
            occurrence distribution.
        :param list|tuple param_values: the parameters that are used for sampling from the occurrence distribution.
        :return: the time step for the scheduled execution of the given event instance
        :rtype: int|None
        """
        res = self.config.occurrence_distribution.generateSample(evaluation_context, param_values)
        assert isinstance(res, int) and not isinstance(res, bool)
        t = evaluation_context.getFluentValue("time")
        return t + res

    def generate_instance(self, evaluation_context, entity_combination):
        """
        Generates a concrete event instance by sampling from the stochastic parameter distributions.
        The result is a raw tuple that can be used in progression.

        :type evaluation_context: EvaluationContext
        :type entity_combination: list

        :rtype: (str, tuple)
        """
        args = list(entity_combination)
        # : :type distribution: Distribution
        for distribution in self.config.stochastic_param_distributions:
            val = distribution.generateSample(evaluation_context, args)
            args.append(val)

        refined_args = list(map(
            lambda a: a.getId() if isinstance(a, Entity) else a,
            args)
        )
        return self.__action_name, tuplify(refined_args)

    def __repr__(self):
        return "ExogenousAction({},{}, {})".format(self.action_name, self.entity_params, self.stochastic_params)

    def __str__(self, *args, **kwargs):
        return "ExogenousAction({},{}, {})".format(self.action_name, self.entity_params, self.stochastic_params)

    def describe(self):
        s = ("Exogenous action: {name}({entity_params}, {stochastic_params})\n"
             "  Occurrence distribution: {occ_distrib}\n"
             "  Parameter Distributions: {p_distribs}\n")
        p_distrib_descs = []
        for p in self.stochastic_params:
            p_distrib_descs.append("{}:={}".format(p[0], self.config.get_param_distribution(p[0]).describe()))

        return s.format(name=self.action_name,
                        entity_params=self.entity_params,
                        stochastic_params=self.stochastic_params,
                        occ_distrib=(
                            self.config.occurrence_distribution.describe()
                            if self.config.occurrence_distribution is not None else "None"
                        ),
                        p_distribs=", ".join(p_distrib_descs)
        )


class EventOptionSelectionStrategy:
    def __init__(self):
        self.__choice = None

    @property
    def choice(self):
        """
        The Choice that uses this selection strategy.
        :rtype: ExogenousActionChoice
        """
        return self.__choice

    @choice.setter
    def choice(self, choice):
        """
        Sets the choice that uses this selection strategy.
        :param ExogenousActionChoice choice: the choice that uses this selection strategy.
        """
        self.__choice = choice

    def make_choice(self, evaluation_context, param_values):
        """
        Chooses one option of the associated ExogenousActionChoice.
        :param EvaluationContext evaluation_context: the evaluation context that will be used to interpret the parameter values.
        :param list param_values: the parameter values.
        """
        raise NotImplementedError()


class ExogenousActionChoice(object):
    """
    Represents a choice of alternative event options of which only one will be scheduled.
    """

    def __init__(self, choice_name, entity_params, options, selection_strategy=None,
                 occurrence_distribution=None):
        """
        Creates a choice. This is mainly done in world.load_declaration().

        :param str choice_name: the name of the choice.
        :param list[(str, str)] entity_params: the parameters that identify the possible event instance options.
        :param list[ExogenousAction] options: the event options.
        :param EventOptionSelectionStrategy selection_strategy: the option selection strategy.
            Default = uniform.
        :param  Distribution occurrence_distribution: the probability distribution that is used to determine
            whether any option of this choice should occur. Default = yes with 100% probability.
        """
        self.__choice_name = choice_name
        self.__entity_params = entity_params
        self.__options = options
        self.__selection_strategy = selection_strategy
        if self.__selection_strategy is None:
            self.selection_strategy = UniformEventOptionSelectionStrategy()

        self.__occurrence_distribution = (occurrence_distribution if occurrence_distribution is not None
                                          else BernoulliDistribution(1.0))

    @property
    def choice_name(self):
        """
        The choice's name.
        :return str: the choice's name.
        """
        return self.__choice_name

    @property
    def entity_params(self):
        """
        The identifying entity parameters of this ExogenousActionChoice.
        :rtype: list[(str, str)]
        """
        return self.__entity_params

    @property
    def options(self):
        """
        The options of this ExogenousActionChoice.
        :rtype: list[ExogenousAction]
        """
        return self.__options

    @property
    def selection_strategy(self):
        """
        The option selection strategy.
        :rtype: EventOptionSelectionStrategy
        """
        return self.__selection_strategy

    @selection_strategy.setter
    def selection_strategy(self, strategy):
        """
        Sets the new EventOptionSelectionStrategy.
        :param EventOptionSelectionStrategy strategy: the new strategy
        """
        self.__selection_strategy = strategy
        self.__selection_strategy.choice = self

    @property
    def occurrence_distribution(self):
        """
        The probability distribution that is used to determine
            whether any option of this choice should occur
        :rtype: Distribution
        """
        return self.__occurrence_distribution

    @occurrence_distribution.setter
    def occurrence_distribution(self, distribution):
        """
        Sets the probability distribution that is used to determine
            whether any option of this choice should occur
        :type distribution: Distribution
        """
        self.__occurrence_distribution = distribution

    def make_choice(self, evaluation_context, param_values):
        """
        Selects one option.

        :param EvaluationContext evaluation_context: the evaluation context that is used to interpret
            the parameter values.
        :param list|tuple param_values: the parameter values.
        :rtype: ExogenousAction
        """
        return self.selection_strategy.make_choice(evaluation_context, param_values)

    def should_happen(self, evaluation_context, param_values):
        """
        Uses the defined occurrence distribution to decide whether the event instance with the given parameters
        should be executed at the current time step.

        :param EvaluationContext evaluation_context: the evaluation context that is used by the
            occurrence distribution.
        :param list|tuple param_values: the parameters that are used for sampling from the occurrence distribution.
        :return: True if the event instance should be executed at the current time step.
        :rtype: bool
        """
        res = self.occurrence_distribution.generateSample(evaluation_context, param_values)
        assert isinstance(res, bool)
        return res


class UniformEventOptionSelectionStrategy(EventOptionSelectionStrategy):
    """
    An EventOptionSelectionStrategy that uses the same probability for each option.
    """

    def __init__(self):
        super().__init__()

    def make_choice(self, evaluation_context, param_values):
        if self.choice is None:
            raise SALMAException("No choice set for OptionSelectionStrategy {}".format(str(self)))
        return random.choice(self.choice.options)

    def __str__(self):
        return "UniformEventOptionSelectionStrategy"


class StepwiseEventOptionSelectionStrategy(EventOptionSelectionStrategy):
    """
    An event option selection strategy that assigns a fixed probability to each option.
    """

    def __init__(self, probabilities):
        """
        Creates an event option selection strategy with the given properties.
        :param dict[str,  numbers.Real]: probabilities
        :return:
        """
        super().__init__()
        self.__probabilities = probabilities

    @property
    def probabilities(self):
        return self.__probabilities




class ExogenousActionConfiguration:
    def __init__(self, exogenous_action, occurrence_distribution=None, stochastic_param_distribution_specs=None):
        """
        Holds the configuration for an exogenous action.

        :param salma.model.events.ExogenousAction exogenous_action: the exogenous action this configuration is meant for
        :param Distribution occurrence_distribution: Distribution that receives a combination of entity values
                as parameters and decides whether the exogenous action should occur for this combination
        :param list[(str, Distribution)] stochastic_param_distribution_specs: a list of (name, distribution)
                tuples that specify the distributions for the non-entity parameters that will be sampled after
                the exogenous action instance has been chosen to happen.
        """
        if not stochastic_param_distribution_specs:
            stochastic_param_distribution_specs = []
        self.__exogenous_action = exogenous_action
        self.__occurrence_distribution = (occurrence_distribution if occurrence_distribution is not None
                                          else BernoulliDistribution(0.0))

        self.__stochastic_param_distributions = []
        # initialize with uniform distributions with default settings
        for p in self.__exogenous_action.stochastic_params:
            self.__stochastic_param_distributions.append(UniformDistribution(p[1]))
        # add distributions from constructor argument
        for p_spec in stochastic_param_distribution_specs:
            i = self.__exogenous_action.get_stochastic_parameter_index(p_spec[0])
            self.__stochastic_param_distributions[i] = p_spec[1]

    @property
    def exogenous_action(self):
        """
        :rtype: salma.model.events.ExogenousAction
        """
        return self.__exogenous_action

    def check(self):
        problems = []
        if self.__exogenous_action.config is not self:
            raise SALMAException(
                "Inconsistent exogenous action configuration: config of exogenous action {} points to different "
                "ExogenousActionConfig.".format(self.__exogenous_action.action_name))

        if self.__occurrence_distribution is None:
            problems.append(
                "No occurrence probability distribution specified for exogenous action %s ." %
                self.__exogenous_action.action_name)
        else:
            assert isinstance(self.__occurrence_distribution, Distribution)
            # TODO: check for type according to event type (ad hoc | scheduled)
            # if self.occurrence_distribution.sort != "boolean":
            # problems.append(
            # "Wrong type for occurrence distribution of
            # exogenous action {}: was {} but must be boolean.".format(
            # self.exogenous_action.action_name, self.occurrence_distribution.sort
            # ))

        if len(self.__stochastic_param_distributions) != len(self.exogenous_action.stochastic_params):
            return [
                "Wrong number of stochastic parameters for exogenous action {}: expected {} but was {}.".format(
                    self.exogenous_action.action_name,
                    len(self.exogenous_action.stochastic_params),
                    len(self.__stochastic_param_distributions))]

        wrong_types = []
        for i in range(len(self.__stochastic_param_distributions)):
            if self.__stochastic_param_distributions[i].sort != self.exogenous_action.stochastic_params[i][1]:
                wrong_types.append(
                    "Wrong type for stochastic parameter no. {} of exogenous action {}: "
                    "expected {} but was {}.".format(
                        i, self.exogenous_action.action_name,
                        self.exogenous_action.stochastic_params[i][1],
                        self.__stochastic_param_distributions[i].sort
                    ))
        return wrong_types

    @property
    def occurrence_distribution(self):
        """
        The distribution that determines whether or not an exogenous action should happen.
        :rtype: Distribution
        """
        return self.__occurrence_distribution

    @occurrence_distribution.setter
    def occurrence_distribution(self, distribution):
        """
        :type distribution: Distribution
        """
        self.__occurrence_distribution = distribution

    @property
    def stochastic_param_distributions(self):
        """
        :rtype: list of Distribution
        """
        return self.__stochastic_param_distributions

    def get_param_distribution(self, param_name):
        """
        :type param_name: str
        :rtype: Distribution
        """
        i = self.exogenous_action.get_stochastic_parameter_index(param_name)
        return self.__stochastic_param_distributions[i]

    def set_param_distribution(self, param_name, distribution):
        """
        Sets the distribution for the parameter with the given name.
        :type param_name: str
        :type distribution: Distribution
        :rtype: ExogenousActionConfiguration
        """
        i = self.exogenous_action.get_stochastic_parameter_index(param_name)
        self.__stochastic_param_distributions[i] = distribution
        return self

    def uniform_param(self, param_name, value_range=None):
        """
        Defines a uniform distribution for the given parameter.
        :type param_name: str
        :type value_range: tuple
        :rtype: ExogenousActionConfiguration
        """
        i = self.exogenous_action.get_stochastic_parameter_index(param_name)
        p_sort = self.exogenous_action.stochastic_params[i][1]
        d = UniformDistribution(p_sort, value_range)
        self.__stochastic_param_distributions[i] = d
        return self

    def bernoulli_param(self, param_name, probability):
        """
        Defines a bernoulli distribution for the given parameter.
        :type param_name: str
        :type probability: float
        :rtype: ExogenousActionConfiguration
        """
        i = self.exogenous_action.get_stochastic_parameter_index(param_name)
        d = BernoulliDistribution(probability)
        self.__stochastic_param_distributions[i] = d
        return self

    def normal_param(self, param_name, mu, sigma):
        """
        Defines a normal distribution for the given parameter.
        :param param_name: the parameter name
        :param mu: mu
        :param sigma: sigma
        :return: self for chaining

        :type param_name: str
        :type mu: float
        :type sigma: float
        :rtype: ExogenousActionConfiguration
        """
        i = self.exogenous_action.get_stochastic_parameter_index(param_name)
        p_sort = self.exogenous_action.stochastic_params[i][1]
        d = NormalDistribution(p_sort, mu, sigma)
        self.__stochastic_param_distributions[i] = d
        return self

    def set_probability(self, prob):
        """
        A shortcut to set the occurrence probability.

        :param float prob: the new occurrence probability
        :rtype: ExogenousActionConfiguration
        """
        self.occurrence_distribution = BernoulliDistribution(prob)
        return self