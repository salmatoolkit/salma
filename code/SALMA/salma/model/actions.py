from salma.SALMAException import SALMAException
from salma.model.core import Action, Entity
from salma.model.distributions import Distribution, ArgumentIdentityDistribution, UniformDistribution, \
    NormalDistribution, BernoulliDistribution
from salma.model.evaluationcontext import EvaluationContext
from salma.termutils import tuplify
from salma.model.selectionstrategy import OutcomeSelectionStrategy, Uniform


class DeterministicAction(Action):
    """
    Represents a deterministic (primitive) action. The action can be atomic, which means that
    it will be progressed immediately in the simulation loop instead of being interleaved with others.
    """

    def __init__(self, name, parameters, atomic, break_on_failure=False):
        """
        Creates a deterministic action with the given name, parameters and atomicity setting.

        :param str name: the action's name
        :param list[(str, str)]|tuple[(str,str)] parameters: the actions's parameters as list of (name, sort) tuples.
        :param bool atomic: whether ot not the action is atomic
        """
        Action.__init__(self, name, parameters)
        self.__atomic = atomic
        self.__break_on_failure = break_on_failure

    @property
    def atomic(self):
        return self.__atomic

    @property
    def break_on_failure(self) -> bool:
        """
        If break_on_failure is true, the experiment run will be canceled when an instance of this action fails.
        """
        return self.__break_on_failure

    @break_on_failure.setter
    def break_on_failure(self, bof: bool):
        self.__break_on_failure = bof

    def __str__(self):
        return "DeterministicAction: {}({})".format(self.name, self.parameters, self.__atomic)

    def describe(self):
        return "DeterministicAction: {}({})".format(self.name, self.parameters, self.__atomic)


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
        :type param_distribution_specs: list[(str, object)]
        """
        if not param_distribution_specs:
            param_distribution_specs = []
        # : :type : DeterministicAction
        self.__outcome_action = outcome_action
        # : :type : StochasticAction
        self.__stochastic_action = None

        #: :type : list[Distribution]
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
        :type param_values: list|set|tuple
        :rtype: (str, tuple)
        """
        args = []

        for p_dist in self.__param_distributions:
            # generateSample(self, domainMetaModel, paramValues):
            val = p_dist.generateSample(evaluation_context, param_values)
            if isinstance(val, Entity):
                args.append(val.id)
            else:
                args.append(val)

        return self.__outcome_action.name, tuplify(args)

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
            if self.param_distributions[i] is None:
                problems.append(("random_action_outcome.undefined_param_distribution",
                                 self.action_name, i))
            elif self.param_distributions[i].sort != action.parameters[i][1]:
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


class StochasticAction(Action):
    """
    Represents a stochastic action, i.e. an action that has multiple possible deterministic actions as outcomes.
    The outcome is chosen by the simulation engine according to a selection strategy that is usually
    a categorical distribution which assigns a probability to each option.
    """

    def __init__(self, name, parameters, outcomes, selection_strategy=None):
        """
        Creates a stochastic action. Action instances are created when the declaration is read and
        added to the world's action registry. The outcome and parameter distribution can then be set with the
        associated configuration.

        :param str name: the name of the stochastic action
        :param list[(str, str)] parameters: the stochastic action's parameters
        :param list[RandomActionOutcome] outcomes: list of RandomActionOutcome objects
        :param OutcomeSelectionStrategy|None selection_strategy: the outcome selection strategy
        """
        Action.__init__(self, name, parameters)

        # : :type: dict[str, RandomActionOutcome]
        self.__outcomes = dict()
        #: :type: list[RandomActionOutcome]
        self.__outcome_list = []
        for outcome in outcomes:
            self.__outcomes[outcome.action_name] = outcome
            outcome.stochastic_action = self
            self.__outcome_list.append(outcome)
        #: :type: OutcomeSelectionStrategy
        self.__selection_strategy = None
        if selection_strategy is not None:
            self.selection_strategy = selection_strategy
        else:
            self.selection_strategy = Uniform()

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
            self.__selection_strategy.set_option_provider(None)
        self.__selection_strategy = strategy
        if self.__selection_strategy is not None:
            self.__selection_strategy.set_option_provider(lambda: self.__outcomes)

    @property
    def outcomes(self):
        """
        :rtype: list[RandomActionOutcome]
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

    def generate_outcome(self, evaluation_context, param_values):
        """
        Returns a concrete sample as a (action, params) tuple.

        :type evaluation_context: EvaluationContext
        :type param_values: list|tuple|set
        :rtype: (str, tuple)
        """
        outcome = self.selection_strategy.select_outcome(evaluation_context, param_values)
        assert isinstance(outcome, RandomActionOutcome)
        return outcome.generate_sample(evaluation_context, param_values)

    def check(self, action_dict):
        """
        Checks if the outcome selection configuration and the outcomes are configured properly.
        :return: a list of problems
        :param dict[str, Action] action_dict: the world's action registry.
        :rtype: list[(str, tuple)]
        """
        if self.__outcomes is None or len(self.__outcomes) == 0:
            return [("stochastic_action.no_outcome", ())]
        if self.selection_strategy is None:
            return [("stochastic_action.no_selection_strategy", ())]
        problems = []
        problems.extend(self.selection_strategy.check())
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


