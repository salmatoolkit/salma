from argparse import _ActionsContainer
import random
from salma.SMCException import SMCException
from salma.model.core import Action, Entity
from salma.model.distributions import Distribution, ArgumentIdentityDistribution, UniformDistribution, NormalDistribution, BernoulliDistribution
from salma.model.evaluationcontext import EvaluationContext


class DeterministicAction(Action):
    def __init__(self, name, parameter_types, immediate=False):
        Action.__init__(self, name, parameter_types, immediate)

    def __str__(self):
        return "DeterministicAction({},{})".format(self.name, self.parameters)


class RandomActionOutcome(object):
    """
    Determines the action and distributions for each parameter.

    The list of action parameters can contain Distribution instances or sort names. The latter
    will be translated to instances of ArgumentIdentityDistribution with their current position in the argument
    list set as the ArgumentIdentityDistribution's argument position.
    """

    def __init__(self, action_name, param_distribution_specs=[]):
        """
        :param action_name: the action_name
        :param paramDistributionSpecs: list of ParamDistribution objects

        :type action_name: str
        :type param_distribution_specs: list
        """
        self.__actionName = action_name

        self.__param_distribution_specs = []
        for i, param in enumerate(param_distribution_specs):
            dist = (param if isinstance(param, Distribution)
                    else ArgumentIdentityDistribution(param, i))
            self.__param_distribution_specs.append(dist)

    @property
    def action_name(self):
        return self.__actionName

    @property
    def param_distributions(self):
        return self.__param_distribution_specs

    def generate_sample(self, evaluation_context, param_values):
        """
        Generates a concrete outcome as a tuple with the form (action_name, params)
        :type evaluation_context: EvaluationContext
        :type param_values: list of object
        :rtype: (str, list of object)
        """
        args = []

        for pdist in self.__param_distribution_specs:
            # generateSample(self, domainMetaModel, paramValues):
            val = pdist.generateSample(evaluation_context, param_values)
            if isinstance(val, Entity):
                args.append(val.id)
            else:
                args.append(val)

        return self.__actionName, args

    def add_param(self, param):
        """
        Adds a parameter specification. If param is a string instead of a Distribution object, an
        ArgumentIdentityDisribution is created with param used as sort.

        :param param: the parameter to add, either a sort name or a Distribution
        :return: self for chaining

        :rtype: RandomActionOutcome
        """
        i = len(self.__param_distribution_specs)
        dist = (param if isinstance(param, Distribution)
                else ArgumentIdentityDistribution(param, i))
        self.__param_distribution_specs.append(dist)
        return self

    def fixed_param(self, param_sort):
        i = len(self.__param_distribution_specs)
        self.__param_distribution_specs.append(ArgumentIdentityDistribution(param_sort, i))
        return self

    def uniform_param(self, sort, value_range=None):
        d = UniformDistribution(sort, value_range)
        self.__param_distribution_specs.append(d)
        return self

    def bernoulli_param(self, probability):
        d = BernoulliDistribution(probability)
        self.__param_distribution_specs.append(d)
        return self

    def normal_param(self, sort, mu, sigma):
        d = NormalDistribution(sort, mu, sigma)
        self.__param_distribution_specs.append(d)
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
            #:type action: Action
        action = action_dict[self.action_name]
        if len(self.param_distributions) != len(action.parameters):
            return [("random_action_outcome.wrong_param_count",
                     self.action_name, len(action.parameters), len(self.param_distributions))]
        problems = []
        for i in range(len(self.param_distributions)):
            if self.param_distributions[i].sort != action.parameters[i]:
                problems.append(
                    ("random_action_outcome.wrong_param_type",
                     self.action_name, i, action.parameters[i], self.param_distributions[i].sort
                    )
                )
        return problems


NOP_OUTCOME = RandomActionOutcome('nop', [])


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


class StochasticAction(Action):
    def __init__(self, name, parameter_types, outcomes, selection_strategy=None, immediate=False):
        """
        Represents a stochastic action. Action instances are created when the declaration is read and
        added to the world's action registry. The outcome and parameter distribution can then be set with the
        associated configuration.

        :param outcomes: list of RandomActionOutcome objects
        :type name: str
        :type parameter_types: list of str
        :type outcomes: list of RandomActionOutcome
        :type selection_strategy: OutcomeSelectionStrategy
        :type immediate: bool
        """
        Action.__init__(self, name, parameter_types, immediate)
        self.__selection_strategy = selection_strategy
        #: :type: dict of (str, RandomActionOutcome)
        self.__outcomes = dict()

        for outcome in outcomes:
            self.__outcomes[outcome.action_name] = outcome
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
        :raises SMCException if no outcome is registered with the given action name.
        :type action_name: str
        :rtype: RandomActionOutcome
        """
        try:
            return self.__outcomes[action_name]
        except KeyError:
            raise SMCException(
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
        return "StochasticAction({},{})".format(self.name, self.parameters)


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
            problems.append("outcome_selection_strategy.deterministic.more_than_one_outcome")
        return problems


class Uniform(OutcomeSelectionStrategy):
    """
    Creates an outcome selection strategy that chooses one outcome uniformly from the list of outcomes.
    """
    def __init__(self):
        super().__init__()

    def select_outcome(self, evaluationContext, paramValues):
        return random.choice(self.action.outcomes)


class Stepwise(OutcomeSelectionStrategy):
    """
    Creates a selection strategy where a selection probability is given for each outcome.
    """
    def __init__(self, probabilities=[]):
        """
        :param probabilities: a list of (action_name, probability)
        :type probabilities: list of (str, float)
        """
        super().__init__()
        #: :type: dict of (str, float)
        self.__probabilities = dict()
        for p in probabilities:
            self.__probabilities[p[0]] = p[1]

    @property
    def probabilities(self):
        """
        The selection properties for all action outcome.
        :rtype: dict of (str, float)
        """
        return self.__probabilities

    def set_probability(self, action_name, probability):
        """
        Sets the selection probability for the given action.
        :type action_name: str
        :type probability: float
        """
        self.__probabilities[action_name] = probability

    def probability(self, action_name : str) -> float:
        """
        Returns the selection probability for the given action.
        """
        try:
            return self.__probabilities[action_name]
        except KeyError:
            raise SMCException("No probability specified for action {}.".format(action_name))

    def select_outcome(self, evaluation_context, param_values):
        r = random.uniform(0, 1)
        start = 0
        for action, probability in self.__probabilities.items():
            if (r >= start) and (r < start + probability):
                return self.action.outcome(action)
            start += probability
        raise(SMCException("No outcome could be selected, r = {}, probabilities = {}".format(r,self.__probabilities)))

    def check(self, action_dict):
        """
        :type action_dict: dict of (str, Action)
        :rtype: list of tuple
        """
        problems = super().check(action_dict)
        for outcome in self.action.outcomes:
            if not outcome.action_name in self.__probabilities:
                problems.append(("outcome_selection_strategy.stepwise.no_prob_for_outcome", outcome.action_name))

        s = sum(self.__probabilities.values())
        if s != 1.0:
            problems.append(("outcome_selection_strategy.stepwise.wrong_prob_sum", s))

        return problems


class ExogenousAction(object):
    # : :type __qualifyingParamDistributions: list
    __qualifyingParamDistributions = []

    def __init__(self, action_name, entity_param_types, stochastic_param_types, configuration=None):
        """
        action_name: the action_name
        :type action_name: str

        :param entityParams: list of (paramName, sort) tuples

        :type entity_param_types: list
        :type stochastic_param_types: list

        """
        self.__action_name = action_name
        self.__entity_param_types = entity_param_types
        self.__stochastic_param_types = stochastic_param_types
        self.__configuration = configuration if not configuration is None else ExogenousActionConfiguration()


    @property
    def action_name(self):
        return self.__action_name

    @property
    def entity_param_types(self):
        return self.__entity_param_types

    @property
    def stochastic_param_types(self):
        return self.__stochastic_param_types

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
        self.__configuration.exogenous_action = self

    def should_happen(self, evaluation_context, param_values):
        return self.config.occurrence_distribution.generateSample(evaluation_context, param_values)

    def generate_instance(self, evaluation_context, entity_combination):
        """
        :type evaluation_context: EvaluationContext
        :type entity_combination: list
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
        return "ExogenousAction({},{})".format(self.action_name, self.entity_param_types, self.stochastic_param_types)


class ExogenousActionConfiguration:
    def __init__(self, occurrence_distribution=None, stochastic_param_distributions=[]):
        """
        Holds the configuration for an exogenous action.

        :param occurrence_distribution: Distribution that receives a combination of entity values as parameters and decides whether the
            exogeneous action should occur for this combination
        :param stochastic_param_distributions: a list of distributions for the non-entity parameters that will be sampled after
            the exogenous action instance has been chosen to happen.

        :type occurrence_distribution: Distribution
        :type stochastic_param_distributions: list
        """
        self.__exogenous_action = None
        self.__occurrence_distribution = occurrence_distribution
        self.__stochastic_param_distributions = stochastic_param_distributions

    @property
    def exogenous_action(self):
        """
        :rtype: ExogenousAction
        """
        return self.__exogenous_action

    @exogenous_action.setter
    def exogenous_action(self, action):
        """
        :type action: ExogenousAction
        """
        self.__exogenous_action = action

    def check(self):
        problems = []
        if self.__occurrence_distribution is None:
            problems.append(
                "No occurance probability distribution specified for exogenous action %s ." % self.__exogenous_action.action_name)
        if isinstance(self.occurrence_distribution, Distribution):
            if self.occurrence_distribution.sort != "boolean":
                problems.append(
                    "Wrong type for occurrence distribution of exogenous action {}: was {} but must be boolean.".format(
                        self.exogenous_action.action_name, self.occurrence_distribution.sort
                    ))

        if len(self.__stochastic_param_distributions) != len(self.exogenous_action.stochastic_param_types):
            return [
                "Wrong number of stochastic parameters for exogenous action {}: expected {} but was {}.".format(
                    self.exogenous_action.action_name,
                    len(self.exogenous_action.stochastic_param_types),
                    len(self.__stochastic_param_distributions))]

        wrong_types = []
        for i in range(len(self.__stochastic_param_distributions)):
            if self.__stochastic_param_distributions[i].sort != self.exogenous_action.stochastic_param_types[i]:
                wrong_types.append(
                    "Wrong type for stochastic parameter no. {} of exogenous action {}: expected {} but was {}.".format(
                        i, self.exogenous_action.action_name,
                        self.exogenous_action.stochastic_param_types[i],
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
        return self.__stochastic_param_distributions

    @stochastic_param_distributions.setter
    def stochastic_param_distributions(self, distributions):
        """
        :type distributions: list
        """
        self.__stochastic_param_distributions = distributions.copy()

    def add_param(self, distribution):
        """
        :param distribution: the distribution
        :return: self for chaining

        :type distribution: Distribution
        :rtype: ExogenousActionConfiguration
        """
        self.__stochastic_param_distributions.append(distribution)
        return self

    def uniform_param(self, sort, value_range=None):
        d = UniformDistribution(sort, value_range)
        self.__stochastic_param_distributions.append(d)
        return self

    def bernoulli_param(self, probability):
        d = BernoulliDistribution(probability)
        self.__stochastic_param_distributions.append(d)
        return self

    def normal_param(self, sort, mu, sigma):
        d = NormalDistribution(sort, mu, sigma)
        self.__stochastic_param_distributions.append(d)
        return self


