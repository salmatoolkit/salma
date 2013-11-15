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

    def generate_sample(self, evaluationContext, paramValues):
        """
        generates a tuple with the form (action_name, params)
        """
        args = []

        for pdist in self.__param_distribution_specs:
            # generateSample(self, domainMetaModel, paramValues):
            val = pdist.generateSample(evaluationContext, paramValues)
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
        :return: None if everything is OK, otherwise a list of strings describing problems.
        """
        if self.action_name not in action_dict:
            return ["Primitive action %s not registered." % self.action_name]
            #:type action: Action
        action = action_dict[self.action_name]
        if len(self.param_distributions) != len(action.parameters):
            return [
                "Wrong number of parameters for action outcome {}: expected {} but was {}.".format(
                    self.action_name,
                    len(action.parameters),
                    len(self.param_distributions))]

        wrong_types = []
        for i in range(len(self.param_distributions)):
            if self.param_distributions[i].sort != action.parameters[i]:
                wrong_types.append(
                    "Wrong type for parameter no. {} of action outcome {}: expected {} but was {}.".format(
                        i, self.action_name, action.parameters[i], self.param_distributions[i].sort
                    ))
        return wrong_types


NOP_OUTCOME = RandomActionOutcome('nop', [])


class StochasticActionConfiguration:
    def __init__(self):
        self.__action = None
        self.__outcomes = []
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

    def add_outcome(self, outcome):
        """
        Adds a prepared RandomActionOutcome.
        :return: a reference to self for chaining.

        :type outcome: RandomActionOutcome
        :rtype: Uniform
        """
        self.__outcomes.append(outcome)
        return self

    def create_outcome(self, action_name):
        """
        Creates a new unconfigured action outcome for the given promitive action. The outcome
        is added to the outcome list and returned for configuration.

        :param action_name: the name of the primitive action
        :return: the outcome

        :type action_name: str
        :rtype: RandomActionOutcome
        """
        outcome = RandomActionOutcome(action_name)
        self.__outcomes.append(outcome)
        return outcome

    @property
    def outcomes(self):
        """
        :rtype: list
        """
        return self.__outcomes

    @outcomes.setter
    def outcomes(self, outcomes):
        """
        :type outcomes: list
        """
        self.__outcomes = outcomes.copy()

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
        Checks if the configuration is consitent.
        :return: None if consistent or a list of problem description

        :type action_dict: dict
        """
        if self.__outcomes is None or len(self.__outcomes) == 0:
            return ["No outcome specified."]
        problems = []
        for o in self.__outcomes:
            problems.extend(o.check(action_dict))
        return problems


class StochasticAction(Action):
    def __init__(self, name, parameter_types, config=None, immediate=False):
        '''
        Represents a stochastic action. Action instances are created when the declaration is read and
        added to the world's action registry. The outcome and parameter distribution can then be set with the
        associated configuration.

        :type name: str
        :type parameter_types: list
        :type config: StochasticActionConfiguration
        '''
        Action.__init__(self, name, parameter_types, immediate)
        self.__config = config

    @property
    def config(self):
        """
        :return: the configuration object
        :rtype: StochasticActionConfiguration
        """
        return self.__config

    @config.setter
    def config(self, conf):
        """
        :type conf: StochasticActionConfiguration
        """
        if self.__config is not None:
            self.__config.action = None
        self.__config = conf
        self.__config.action = self

    def generateOutcome(self, evaluationContext, paramValues):
        '''
        Returns a concrete sample as a (action, params) tuple.

        :type evaluationContext: EvaluationContext
        :type paramValues: list
        :rtype: tuple
        '''
        outcome = self.config.select_outcome(evaluationContext, paramValues)
        return outcome.generate_sample(evaluationContext, paramValues)

    def __str__(self):
        return "StochasticAction({},{})".format(self.name, self.parameters)


class Parametric(StochasticActionConfiguration):
    def __init__(self, action_name, params=[]):
        """
        Creates a stochastic action outcome selector that is parametric in the sense that there is only one outcome
        specification.

        :param action_name: the name of the action outcome
        :param params: the list of parameters, either sort name or Distribution

        :type action_name: str
        :type params: list
        """
        super().__init__()
        self.outcomes = [RandomActionOutcome(action_name, params)]

    def select_outcome(self, evaluationContext, paramValues):
        return self.outcomes[0]

    def create_outcome(self, action_name):
        self.outcomes = [RandomActionOutcome(action_name)]
        return self.outcomes[0]

    @property
    def outcome(self):
        return self.outcomes[0]


class Uniform(StochasticActionConfiguration):
    def __init__(self, outcomes=None):
        super().__init__()
        if not outcomes is None:
            self.outcomes = outcomes

    def select_outcome(self, evaluationContext, paramValues):
        return random.choice(self.outcomes)

    def add_outcome(self, action_name, params):
        """
        Adds an outcome that is defined by an action name and parameter distributions.
        :return: a reference to self for chaining.

        :type action_name: str
        :type params: list
        :rtype: Uniform
        """
        outcome = RandomActionOutcome(action_name, params)
        return self.add_outcome(outcome)


class Stepwise(StochasticActionConfiguration):
    def __init__(self):
        super().__init__()
        # __outcome_specs is a list of (probability, outcome) tuples.
        self.__outcome_specs = []

    def add_outcome(self, probability, outcome):
        """
        Adds an action outcome with the given probability.

        :param probability: the probability with which outcome occurs.
        :param outcome: the action outcome
        :type probability: int
        :type outcome: RandomActionOutcome

        :rtype: Stepwise
        """
        self.__outcome_specs.append((probability, outcome))
        return self

    def create_outcome(self, action_name, probability):
        outcome = RandomActionOutcome(action_name)
        self.__outcome_specs.append((probability, outcome))
        return outcome

    def select_outcome(self, evaluationContext, paramValues):
        r = random.uniform(0, 1)
        start = 0
        for spec in self.__outcome_specs:
            if (r >= start) and (r < start + spec[0]):
                return spec[1]
            start += spec[0]
        return None

    def check(self, action_dict):
        if self.__outcome_specs is None or len(self.__outcome_specs) == 0:
            return ["No outcome specified."]
        problems = []
        s = sum(map(lambda x: x[0], self.__outcome_specs))
        if s != 1.0:
            problems.append("Invalid probability distribution: probability sum was {} but should be 1.".format(s))
        for spec in self.__outcome_specs:
            problems.extend(spec[1].check(action_dict))
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
        self.__configuration = configuration


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


