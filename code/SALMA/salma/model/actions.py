import random
from salma.SMCException import SMCException
from salma.model.core import Action, Entity
from salma.model.distributions import Distribution, ArgumentIdentityDistribution
from salma.model.evaluationcontext import EvaluationContext


class DeterministicAction(Action):
    def __init__(self, name, parameter_types, immediate=False):
        Action.__init__(self, name, parameter_types, immediate)


class RandomActionOutcome(object):
    '''
    Determines the action and distributions for each parameter.

    The list of action parameters can contain Distribution instances or sort names. The latter
    will be translated to instances of ArgumentIdentityDistribution with their current position in the argument
    list set as the ArgumentIdentityDistribution's argument position.
    '''

    def __init__(self, action_name, param_distribution_specs=[]):
        '''

        :rtype : object
        :type action_name: str
        :type param_distribution_specs: list

        actionName: the actionName
        paramDistributionSpecs: list of ParamDistribution objects

        '''
        self.__actionName = action_name

        self.__param_distribution_specs = []
        for i, param in enumerate(param_distribution_specs):
            distr = (param if isinstance(param, Distribution)
                     else ArgumentIdentityDistribution(i))
            self.__param_distribution_specs.append(distr)

    @property
    def action_name(self):
        return self.__actionName

    @property
    def param_distributions(self):
        return self.__param_distribution_specs

    def generate_sample(self, evaluationContext, paramValues):
        '''
        generates a tuple with the form (actionName, params)
        '''
        args = []

        for pdistrib in self.__param_distribution_specs:
            # generateSample(self, domainMetaModel, paramValues):
            val = pdistrib.generateSample(evaluationContext, paramValues)
            if isinstance(val, Entity):
                args.append(val.id)
            else:
                args.append(val)

        return (self.__actionName, args)

    def add_param(self, param):
        """
        Adds a parameter specification. If param is a string instead of a Distribution object, an
        ArgumentIdentityDisribution is created.

        :param param: the parameter to add, either a sort name or a Distribution
        :return: self for chaining

        :rtype: RandomActionOutcome
        """
        i = len(self.__param_distribution_specs)
        distr = (param if isinstance(param, Distribution)
                 else ArgumentIdentityDistribution(i))
        self.__param_distribution_specs.append(distr)
        return self

NOP_OUTCOME = RandomActionOutcome('nop', [])


class StochasticActionConfiguration:
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

    def check(self):
        """
        Checks if the configuration is consitent.
        :return: None if consistent or a list of problem description
        """
        raise NotImplementedError()


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


class Parametric(StochasticActionConfiguration):
    def __init__(self, action_name, params=None):
        """
        Creates a stochastic action outcome selector that is parametric in the sense that there is only one outcome
        specification.

        :param action_name: the name of the action outcome
        :param params: the list of parameters, either sort name or Distribution

        :type action_name: str
        :type params: list
        """

        super().__init__()
        self.__action_name = action_name
        self.__param_specs = []
        if not params is None:
            for p in params:
                self.__param_specs.append(p)
            self.__outcome = RandomActionOutcome(self.__action_name, self.__param_specs)
        else:
            self.__outcome = None

    def select_outcome(self, evaluationContext, paramValues):
        return self.__outcome

    def add_param(self, param):
        """
        Adds a parameter to the parameters list. The parameter can either be a sort name (str) or a Distribution.
        :return: a reference to self for chaining.

        :rtype: Parametric
        """
        self.__param_specs.append(param)
        self.__outcome = RandomActionOutcome(self.__action_name, self.__param_specs)
        return self

    def check(self):
        if self.__outcome is None:
            return ["No outcome specified."]
        else:
            return None


class Uniform(StochasticActionConfiguration):
    def __init__(self, outcomes=None):
        super().__init__()
        self.__outcomes = []
        if not outcomes is None:
            self.__outcomes.extend(outcomes)

    def __create_outcome_selector(self):
        def __f(evaluationContext, paramValues):
            return random.choice(self.__outcomes)

        return __f

    def add_outcome(self, outcome):
        """
        Adds a prepared RandomActionOutcome.
        :return: a reference to self for chaining.

        :type outcome: RandomActionOutcome
        :rtype: Uniform
        """
        self.__outcomes.append(outcome)
        return self

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

    def check(self):
        if self.__outcomes is None or len(self.__outcomes) == 0:
            return ["No outcome specified."]
        else:
            return None


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

    def select_outcome(self, evaluationContext, paramValues):
        r = random.uniform(0, 1)
        start = 0
        for spec in self.__outcome_specs:
            if (r >= start) and (r < start + spec[0]):
                return spec[1]
            start += spec[0]
        return None

    def check(self):
        if self.__outcome_specs is None or len(self.__outcome_specs) == 0:
            return ["No outcome specified."]
        s = sum(map(lambda x: x[0], self.__outcome_specs))
        if s != 1.0:
            return ["Invalid probability distribution: probability sum was {} but should be 1.".format(s)]
        return None


class ExogenousAction(object):
    # : :type __qualifyingParamDistributions: list
    __qualifyingParamDistributions = []

    def __init__(self, actionName, entity_param_types, qualifying_param_types, configuration = None):
        """
        actionName: the actionName
        :type actionName: str

        :param entityParams: list of (paramName, sort) tuples

        :type entity_param_types: list
        :type qualifying_param_types: list

        """
        self.__actionName = actionName

        self.__entity_param_types = entity_param_types
        self.__qualifying_param_types = qualifying_param_types
        self.__occuranceDistribution = occuranceDistribution

        self.__qualifyingParamDistributions = qualifyingParamDistributions

    @property
    def actionName(self):
        return self.__actionName

    @property
    def entity_param_types(self):
        return self.__entity_param_types

    @property
    def qualifying_param_types(self):
        return self.__qualifying_param_types

    def get_occurance_distribution(self):
        return self.__occuranceDistribution

    def set_occurance_distribution(self, distrib):
        '''
        :type distrib: Distribution
        '''
        self.__occuranceDistribution = distrib

    def get_qualifying_param_distributions(self):
        return self.__qualifyingParamDistributions

    def set_qualifying_param_distributions(self, distribs):
        '''
        :type distribs: list
        '''
        self.__qualifyingParamDistributions = distribs

    def shouldHappen(self, evaluationContext, paramValues):
        return self.__occuranceDistribution.generateSample(evaluationContext, paramValues)

    def generateInstance(self, evaluationContext, entityCombination):
        '''
        :param domainMetaModel: DomainMetaModel
        :param stateContext: StateContext
        '''
        args = list(entityCombination)
        # : :type distribution: Distribution
        for distribution in self.__qualifyingParamDistributions:
            val = distribution.generateSample(evaluationContext, args)
            args.append(val)

        refinedArgs = list(map(
            lambda a: a.getId() if isinstance(a, Entity) else a,
            args)
        )
        return self.__actionName, refinedArgs


class ExogenousActionConfiguration:
    def __init__(self, occurance_distribution = None, stochastic_param_distributions = []):
        """
        Holds the configuration for an exogenous action.

        :param occurance_distribution: Distribution that receives a combination of entity values as parameters and decides whether the
            exogeneous action should occur for this combination
        :param stochastic_param_distributions: a list of distributions for the non-entity parameters that will be sampled after
            the exogenous action instance has been chosen to happen.

        :type occuranceDistribution: Distribution
        :type stochastic_param_distributions: list
        """
        self.__exogenous_action = None
        self.__occurance_distribution = occurance_distribution
        self.__stochastic_param_distributions = stochastic_param_distributions

    @property
    def exogenous_action(self):
        return self.__exogenous_action

    @exogenous_action.setter
    def exogenous_action(self, action):
        self.__exogenous_action = action

    def check(self):
        raise NotImplementedError()


