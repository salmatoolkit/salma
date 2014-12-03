import random
from salma.SALMAException import SALMAException
from salma.model.core import Action, Entity
from salma.model.distributions import Distribution, ArgumentIdentityDistribution, UniformDistribution, \
    NormalDistribution, BernoulliDistribution
from salma.model.evaluationcontext import EvaluationContext


class DeterministicAction(Action):
    def __init__(self, name, parameters, immediate=False):
        Action.__init__(self, name, parameters, immediate)

    def __str__(self):
        return "DeterministicAction: {}({})".format(self.name, self.parameters)

    def describe(self):
        return "DeterministicAction: {}({})".format(self.name, self.parameters)


class RandomActionOutcome(object):
    """
    Determines the action and distributions for each parameter.

    The list of action parameters can contain Distribution instances or sort names. The latter
    will be translated to instances of ArgumentIdentityDistribution with their current position in the argument
    list set as the ArgumentIdentityDistribution's argument position.
    """

    def __init__(self, outcome_action, param_distribution_specs=None):
        """
        :param outcome_action: the deterministic action that will be executed for this outcome
        :param param_distribution_specs: list of (name, Distribution) or (name, sort) tuples

        :type outcome_action: DeterministicAction
        :type param_distribution_specs: list of (str, object)
        """
        if not param_distribution_specs:
            param_distribution_specs = []
        # : :type : DeterministicAction
        self.__outcome_action = outcome_action
        # : :type : StochasticAction
        self.__stochastic_action = None

        #: :type : list of Distribution
        self.__param_distributions = []

        # create default uniform distributions with Distribution's default value range
        for p in outcome_action.parameters:
            self.__param_distributions.append(UniformDistribution(p[1]))

        for i, param in enumerate(param_distribution_specs):
            index = self.__outcome_action.get_parameter_index(param[0])
            if isinstance(param[1], Distribution):
                dist = param[1]
            else:
                # if the second argument in the tuple is not a distribution, it has to be a sort name
                dist = ArgumentIdentityDistribution(param[1], i)
            self.__param_distributions[index] = dist

    @property
    def action_name(self):
        """
        The name of the deterministic action that will be executed for this outcome.
        :rtype: str
        """
        return self.__outcome_action.name

    @property
    def outcome_action(self):
        """
        The deterministic action that will be executed for this outcome.
        :rtype: DeterministicAction
        """
        return self.__outcome_action

    @property
    def param_distributions(self):
        """
        The list of Distributions for the action's parameters.
        :rtype: list of Distribution
        """
        return self.__param_distributions

    @property
    def stochastic_action(self):
        """
        The stochastic action for which this outcome is defined.
        :rtype: StochasticAction
        """
        return self.__stochastic_action

    @stochastic_action.setter
    def stochastic_action(self, sa):
        """
        :type sa: StochasticAction
        """
        self.__stochastic_action = sa

    def generate_sample(self, evaluation_context, param_values):
        """
        Generates a concrete outcome as a tuple with the form (action_name, params)
        :type evaluation_context: EvaluationContext
        :type param_values: list of object
        :rtype: (str, list of object)
        """
        args = []

        for p_dist in self.__param_distributions:
            # generateSample(self, domainMetaModel, paramValues):
            val = p_dist.generateSample(evaluation_context, param_values)
            if isinstance(val, Entity):
                args.append(val.id)
            else:
                args.append(val)

        return self.__outcome_action.name, args

    def set_param_distribution(self, param_name, distribution):
        """
        Sets the distribution for a parameter.
        :return: self for chaining

        :type param_name: str
        :type distribution: Distribution
        :rtype: RandomActionOutcome
        """
        i = self.__outcome_action.get_parameter_index(param_name)
        self.__param_distributions[i] = distribution
        return self

    def map_param(self, stochastic_action_param, outcome_param):
        """
        Defines a mapping from the given parameter of the stochastic action to the given parameter of the
         outcome's deterministic action.
        :type stochastic_action_param: str
        :type outcome_param: str
        :rtype: RandomActionOutcome
        """
        if self.__stochastic_action is None:
            raise SALMAException("No stochastic action defined for outcome {}.".format(self.action_name))
        i1 = self.stochastic_action.get_parameter_index(stochastic_action_param)
        i2 = self.outcome_action.get_parameter_index(outcome_param)
        param_sort = self.outcome_action.parameters[i2][1]
        self.__param_distributions[i2] = ArgumentIdentityDistribution(param_sort, i1)
        return self

    def uniform_param(self, outcome_param, value_range=None):
        """
        Defines a uniform distribution for the given parameter.
        :type outcome_param: str
        :rtype: RandomActionOutcome
        """
        i = self.outcome_action.get_parameter_index(outcome_param)
        param_sort = self.outcome_action.parameters[i][1]
        d = UniformDistribution(param_sort, value_range)
        self.__param_distributions[i] = d
        return self

    def bernoulli_param(self, outcome_param, probability):
        i = self.outcome_action.get_parameter_index(outcome_param)
        d = BernoulliDistribution(probability)
        self.__param_distributions[i] = d
        return self

    def normal_param(self, outcome_param, mu, sigma):
        i = self.outcome_action.get_parameter_index(outcome_param)
        param_sort = self.outcome_action.parameters[i][1]
        d = NormalDistribution(param_sort, mu, sigma)
        self.__param_distributions[i] = d
        return self

    def check(self, action_dict):
        """
        Checks if the parameter distributions are consistent with the declaration of the primitive action.
        :param action_dict: a dict that contains all declared actions referenced by name.
        :return: Empty list if everything is OK, otherwise a list of strings describing problems.

        :type action_dict: dict of (str, Action)
        :rtype: list of tuple
        """
        if self.action_name not in action_dict:
            return [("random_action_outcome.unregistered_action", self.action_name)]
            # :type action: Action
        action = action_dict[self.action_name]
        if len(self.param_distributions) != len(action.parameters):
            return [("random_action_outcome.wrong_param_count",
                     self.action_name, len(action.parameters), len(self.param_distributions))]
        problems = []
        for i in range(len(self.param_distributions)):
            if self.param_distributions[i].sort != action.parameters[i][1]:
                problems.append(
                    ("random_action_outcome.wrong_param_type",
                     self.action_name, i, action.parameters[i][1], self.param_distributions[i].sort)
                )
        return problems

    def describe(self):
        """
        Returns a textual description of the action outcome.
        :rtype: str
        """
        param_descriptions = []
        for p, distr in zip(self.__outcome_action.parameters, self.__param_distributions):
            param_descriptions.append("{}:={}".format(p[0], distr.describe()))
        return "{}({})".format(self.action_name, ", ".join(param_descriptions))


class OutcomeSelectionStrategy:
    def __init__(self):
        self.__action = None
        pass

    @property
    def action(self):
        """
        The action that is configured by this configuration.
        :rtype: StochasticAction
        """
        return self.__action

    @action.setter
    def action(self, action):
        """
        :type action: StochasticAction
        """
        self.__action = action

    def select_outcome(self, evaluationContext, paramValues):
        """
        Selects a RandomActionOutcome.
        :param evaluationContext: the evaluation context
        :param paramValues: the parameter values

        :return: the action outcome

        :type evaluationContext: EvaluationContext
        :type paramValues: list
        :rtype: RandomActionOutcome
        """
        raise NotImplementedError()

    def check(self, action_dict):
        """
        Checks if the configuration is consistent.
        :return: None if consistent or a list of problem description

        :type action_dict: dict of (str, Action)
        :rtype: list of tuple
        """
        return []

    def describe(self):
        """
        Returns a textual representation of the outcome selection strategy.
        :rtype: str
        """
        return "n/a"


class StochasticAction(Action):
    # TODO remove immediate action stuff
    def __init__(self, name, parameters, outcomes, selection_strategy=None, immediate=False):
        """
        Represents a stochastic action. Action instances are created when the declaration is read and
        added to the world's action registry. The outcome and parameter distribution can then be set with the
        associated configuration.

        :param str name: the name of the stochastic action
        :param list[(str, str)] parameters: the stochastic action's parameters
        :param list[RandomActionOutcome] outcomes: list of RandomActionOutcome objects
        :param OutcomeSelectionStrategy selection_strategy: the outcome selection strategy
        :param bool immediate: whether or not this action is an immediate action (TODO: obsolete)
        """
        Action.__init__(self, name, parameters, immediate)
        self.__selection_strategy = selection_strategy
        # : :type: dict of (str, RandomActionOutcome)
        self.__outcomes = dict()

        for outcome in outcomes:
            self.__outcomes[outcome.action_name] = outcome
            outcome.stochastic_action = self
        self.__outcome_list = list(self.__outcomes.values())

    @property
    def selection_strategy(self):
        """
        :return: the configuration object
        :rtype: OutcomeSelectionStrategy
        """
        return self.__selection_strategy

    @selection_strategy.setter
    def selection_strategy(self, strategy):
        """
        :type strategy: OutcomeSelectionStrategy
        """
        if self.__selection_strategy is not None:
            self.__selection_strategy.action = None
        self.__selection_strategy = strategy
        self.__selection_strategy.action = self

    @property
    def outcomes(self):
        """
        :rtype: list of RandomActionOutcome
        """
        return self.__outcome_list

    def outcome(self, action_name):
        """
        Returns the outcome for the given action name.
        :raises SALMAException if no outcome is registered with the given action name.
        :type action_name: str
        :rtype: RandomActionOutcome
        """
        try:
            return self.__outcomes[action_name]
        except KeyError:
            raise SALMAException(
                "No outcome with action name {} has been declared for stochastic action {}.".format(
                    action_name, self.name)
            )

    def add_outcome(self, outcome):
        """
        Adds an outcome.
        NOTE: This action should be used very carefully since it introduces deviance from the domain
        specification.

        :type outcome: RandomActionOutcome
        """
        self.__outcomes[outcome.action_name] = outcome
        self.__outcome_list = list(self.__outcomes.values())
        outcome.stochastic_action = self

    def generateOutcome(self, evaluation_context, param_values):
        """
        Returns a concrete sample as a (action, params) tuple.

        :type evaluation_context: EvaluationContext
        :type param_values: list of object
        :rtype: (str, list of object)
        """
        outcome = self.selection_strategy.select_outcome(evaluation_context, param_values)
        return outcome.generate_sample(evaluation_context, param_values)

    def check(self, action_dict):
        """
        Checks if the outcome selection configuration and the outcomes are configured properly.
        :return: a list of problems
        :type action_dict: dict of (str, Action)
        :rtype: list of tuple
        """
        if self.__outcomes is None or len(self.__outcomes) == 0:
            return ["stochastic_action.no_outcome"]
        if self.selection_strategy is None:
            return ["stochastic_action.no_selection_strategy"]
        problems = []
        problems.extend(self.selection_strategy.check(action_dict))
        for o in self.__outcomes.values():
            problems.extend(o.check(action_dict))
        return problems

    def __str__(self):
        return "StochasticAction: {}({})".format(self.name, self.parameters)

    def describe(self):
        outcome_descriptions = [o.describe() for o in self.outcomes]
        pstrs = ["{}:{}".format(p[0], p[1]) for p in self.parameters]
        s = """
StochasticAction: {name}({params})
   Outcomes: {outcomes}
   Selection Strategy: {sel_strat}
"""
        return s.format(name=self.name, params=", ".join(pstrs), outcomes=", ".join(outcome_descriptions),
                        sel_strat=self.selection_strategy.describe())


class Deterministic(OutcomeSelectionStrategy):
    """
    Creates a trivial deterministic stochastic action outcome selector that is parametric in the sense that there
    is only one outcome specification.
    """

    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluationContext, paramValues):
        return self.action.outcomes[0]

    def check(self, action_dict):
        problems = super().check(action_dict)
        if len(self.action.outcomes) != 1:
            problems.append(("outcome_selection_strategy.deterministic.more_than_one_outcome", None))
        return problems

    def describe(self):
        return "Deterministic({})".format(self.action.outcomes[0].describe())


class Uniform(OutcomeSelectionStrategy):
    """
    Creates an outcome selection strategy that chooses one outcome uniformly from the list of outcomes.
    """

    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluationContext, paramValues):
        return random.choice(self.action.outcomes)

    def describe(self):
        return "Uniform"


class Stepwise(OutcomeSelectionStrategy):
    """
    Creates a selection strategy where a selection probability is given for each outcome.
    """

    def __init__(self, *args, **kwargs):
        """
        :param list[(str, float)] probabilities: a list of (action_name, probability)
        """
        super().__init__()
        # : :type: dict of (str, float)
        self.__probabilities = dict()
        # alternative 1: one list of tuples
        if len(args) == 1 and isinstance(args[0], list):
            for p in args[0]:
                self.__probabilities[p[0]] = p[1]
        else:
            for outcome, prob in kwargs.items():
                self.__probabilities[outcome] = prob

    @property
    def probabilities(self):
        """
        The selection properties for all action outcome.
        :rtype: dict[str, float]
        """
        return self.__probabilities

    def set_probability(self, action_name, probability):
        """
        Sets the selection probability for the given action.
        :type action_name: str
        :type probability: float
        """
        self.__probabilities[action_name] = probability

    def probability(self, action_name: str) -> float:
        """
        Returns the selection probability for the given action.
        """
        try:
            return self.__probabilities[action_name]
        except KeyError:
            raise SALMAException("No probability specified for action {}.".format(action_name))

    def select_outcome(self, evaluation_context, param_values):
        r = random.uniform(0, 1)
        start = 0
        for action, probability in self.__probabilities.items():
            if (r >= start) and (r < start + probability):
                return self.action.outcome(action)
            start += probability
        raise (
            SALMAException("No outcome could be selected, r = {}, probabilities = {}".format(r, self.__probabilities)))

    def check(self, action_dict):
        """
        :type action_dict: dict of (str, Action)
        :rtype: list of tuple
        """
        problems = super().check(action_dict)
        for outcome in self.action.outcomes:
            if outcome.action_name not in self.__probabilities:
                problems.append(("outcome_selection_strategy.stepwise.no_prob_for_outcome", outcome.action_name))

        s = sum(self.__probabilities.values())
        if s != 1.0:
            problems.append(("outcome_selection_strategy.stepwise.wrong_prob_sum", s))

        return problems

    def describe(self):
        probs = []
        for action, prob in self.probabilities.items():
            probs.append("{}:{:.4}".format(action, prob))
        return "Stepwise({})".format(", ".join(probs))


class ExogenousAction(object):
    """
    Represents an exogenous action, i.e. an event from the environment.
    """

    def __init__(self, action_name, entity_params, stochastic_params, configuration=None):
        """
        :param action_name: the action_name
        :param entity_params and stochastic_params: list of (param-name, sort) tuples

        :type action_name: str
        :type entity_params: list of (str, str)
        :type stochastic_params: list of (str, str)
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
        :param list param_values: the parameters that are used for sampling from the occurrence distribution.
        :return: True if the event instance should be executed at the current time step.
        :rtype: bool
        """
        res = self.config.occurrence_distribution.generateSample(evaluation_context, param_values)
        assert isinstance(res, bool)
        return res

    def get_next_occurrence_time(self, evaluation_context, param_values):
        """
        Uses the defined occurrence distribution to determine the next time step when the
        event instance owoth the given parameters should be executed.

        :param EvaluationContext evaluation_context: the evaluation context that is used by the
            occurrence distribution.
        :param list param_values: the parameters that are used for sampling from the occurrence distribution.
        :return: the time step for the scheduled execution of the given event instance
        :rtype: int
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

        :rtype: (str, list)
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
        return self.__action_name, refined_args

    def __str__(self):
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


class ExogenousActionConfiguration:
    def __init__(self, exogenous_action, occurrence_distribution=None, stochastic_param_distribution_specs=None):
        """
        Holds the configuration for an exogenous action.

        :param ExogenousAction exogenous_action: the exogenous action this configuration is meant for
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
        :rtype: ExogenousAction
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

