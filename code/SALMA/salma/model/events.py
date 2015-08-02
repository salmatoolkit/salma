from salma.SALMAException import SALMAException
from salma.model.core import Entity
from salma.model.distributions import BernoulliDistribution, UniformDistribution, Distribution, NormalDistribution, \
    NEVER, DONT_OCCUR
from salma.model.evaluationcontext import EvaluationContext
from salma.termutils import tuplify
from salma.model.selectionstrategy import OutcomeSelectionStrategy, NonDeterministic


class ExogenousAction(object):
    SCHEDULABLE, AD_HOC, CHOICE_OPTION, CAUSED = range(4)
    TYPE_MAP = {"schedulable": SCHEDULABLE, "ad_hoc": AD_HOC, "choice_option": CHOICE_OPTION,
                "caused": CAUSED}

    """
    Represents an exogenous action, i.e. an event from the environment.
    """

    def __init__(self, action_name, entity_params, stochastic_params, scheduling_type, configuration=None):
        """
        :param str action_name: the action_name
        :param list[(str, str)] entity_params: list of (param-name, sort) tuples describing entity parameters.
        :param list[(str, str)] stochastic_params: list of (param-name, sort) tuples describing stochastic parameters.
        :param str scheduling_type: the event type (one of schedulable, ad_hoc, and choice_option)
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

        try:
            self.__scheduling_type = ExogenousAction.TYPE_MAP[scheduling_type]
        except KeyError:
            raise SALMAException("Wrong scheduling type for exogenous action {}: {}".format(action_name,
                                                                                            scheduling_type))
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

    @property
    def scheduling_type(self):
        """
        The scheduling type of this exogenous action, i.e. one of ExogenousAction.SCHEDULED,
        ExogenousAction.AD_HOC, ExogenousAction.CHOICE_OPTION
        :rtype: int
        """
        return self.__scheduling_type

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
        if res is None:
            return None
        assert isinstance(res, int) and not isinstance(res, bool)
        t = evaluation_context.get_fluent_value("time")
        assert isinstance(t, int)
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
                        p_distribs=", ".join(p_distrib_descs))


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
        :param OutcomeSelectionStrategy|None selection_strategy: the outcome selection strategy.
            Default = uniform.
        :param Distribution occurrence_distribution: the probability distribution that is used to determine
            whether any option of this choice should occur. Default = yes with 100% probability.
        """
        self.__choice_name = choice_name
        self.__entity_params = entity_params
        #: :type: dict[str, ExogenousAction]
        self.__options = dict()
        #: :type: list[ExogenousAction]
        self.__option_list = []
        for o in options:
            self.__options[o.action_name] = o
            self.__option_list.append(o)

        #: :type: OutcomeSelectionStrategy
        self.__selection_strategy = None
        if selection_strategy is not None:
            self.selection_strategy = selection_strategy
        else:
            self.selection_strategy = NonDeterministic()

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
        return self.__option_list

    def option(self, name):
        """
        Returns the outcome with the given name.
        :param str name: the outcome name.
        :rtype: ExogenousAction
        """
        try:
            return self.__options[name]
        except KeyError:
            raise SALMAException("No option with name {} specified for ExogenousActionChoice {}".format(
                name, self.choice_name
            ))

    @property
    def selection_strategy(self):
        """
        The option selection strategy.
        :rtype: OutcomeSelectionStrategy
        """
        return self.__selection_strategy

    @selection_strategy.setter
    def selection_strategy(self, strategy):
        """
        Sets the new EventOptionSelectionStrategy.
        :param OutcomeSelectionStrategy strategy: the new strategy
        """
        if self.__selection_strategy is not None:
            self.__selection_strategy.set_option_provider(None)
        self.__selection_strategy = strategy
        self.__selection_strategy.set_option_provider(lambda: self.__options)

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
        outcome = self.selection_strategy.select_outcome(evaluation_context, param_values)
        assert isinstance(outcome, ExogenousAction)
        return outcome

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

    def __repr__(self, *args, **kwargs):
        return "ExogenousActionChoice({},{})".format(self.choice_name,
                                                     [o.action_name for o in self.options])


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
        self.__occurrence_distribution = occurrence_distribution

        self.__stochastic_param_distributions = []
        # initialize slots
        for _ in self.__exogenous_action.stochastic_params:
            self.__stochastic_param_distributions.append(None)
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
        """
        Checks the event configuration.
        - is the occurrence distribution defined?
        - does the occurrence distribution have the right type?
        - is a correct distribution configured for every stochastic parameter?

        NOTE: the parameter check is omitted when the event is deactivated by setting the occurrence distribution
        to NEVER or DONT_OCCUR

        :return: list of tuples with (error_id, event_name[, expected, actual])
        :rtype: list[tuple]
        """
        if self.__exogenous_action.config is not self:
            raise SALMAException(
                "Inconsistent exogenous action configuration: config of exogenous action {} points to different "
                "ExogenousActionConfig.".format(self.__exogenous_action.action_name))

        if self.__occurrence_distribution is None:
            return [("exogenous_action.undefined_ocurrence_distribution", self.exogenous_action.action_name)]

        else:
            assert isinstance(self.__occurrence_distribution, Distribution)
            if self.__exogenous_action.scheduling_type in [ExogenousAction.SCHEDULABLE,
                                                           ExogenousAction.CHOICE_OPTION,
                                                           ExogenousAction.CAUSED]:
                expected = "integer"

            else:  # AD_HOC
                expected = "boolean"
            if self.__occurrence_distribution.sort != expected:
                return [("exogenous_action.wrong_occurence_distribution_type",
                         self.exogenous_action.action_name, expected, self.__occurrence_distribution.sort)]

        if self.__occurrence_distribution in [NEVER, DONT_OCCUR]:
            return []

        if len(self.__stochastic_param_distributions) != len(self.exogenous_action.stochastic_params):
            return [("exogenous_action.wrong_stochastic_param_count",
                     self.exogenous_action.action_name,
                     len(self.exogenous_action.stochastic_params),
                     len(self.__stochastic_param_distributions))]

        parameter_problems = []
        for i in range(len(self.__stochastic_param_distributions)):
            if self.__stochastic_param_distributions[i] is None:
                parameter_problems.append(("exogenous_action.undefined_stochastic_param_distribution",
                                           self.exogenous_action.action_name, i))
            elif self.__stochastic_param_distributions[i].sort != self.exogenous_action.stochastic_params[i][1]:
                parameter_problems.append(
                    ("exogenous_action.wrong_stochastic_param_distribution_sort",
                     self.exogenous_action.action_name, i,
                     self.exogenous_action.stochastic_params[i][1],
                     self.__stochastic_param_distributions[i].sort))
        return parameter_problems

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


class EventOccurrence:
    def __init__(self, time_point, event, params):
        """
        Defines an event occurrence at the given time point.
        :param int time_point: the time step at which the given event instance occurs.
        :param ExogenousAction|ExogenousActionChoice event: the event.
        :param list|tuple params: the parameters of the event occurrence.
        """
        self.__time_point = time_point
        self.__event = event
        self.__params = params

    @property
    def time_point(self):
        return self.__time_point

    @property
    def event(self):
        return self.__event

    @property
    def params(self):
        return self.__params

    def __eq__(self, other):
        if not isinstance(other, EventOccurrence):
            return False
        else:
            return ((self.__time_point, self.__event, self.__params) ==
                    (other.__time_point, other.__event, other.__params))

    def __hash__(self):
        return hash((self.__time_point, self.__event, self.__params))

    def __lt__(self, other):
        if not isinstance(other, EventOccurrence):
            raise TypeError("Can't compare EventOccurrence to {}".format(type(other)))
        else:
            return self.time_point < other.time_point

    def __repr__(self):
        return "EventOccurrence({}, {}, {})".format(self.__time_point, self.__event, self.__params)


