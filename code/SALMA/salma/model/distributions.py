import numpy as np
from salma.SALMAException import SALMAException
from salma.model.evaluationcontext import EvaluationContext
from numbers import Real


class Distribution(object):
    DEFAULT_MAX_VALUE = 1000
    DEFAULT_MIN_VALUE = 0

    def __init__(self, sort, value_range=None):
        self.__sort = sort
        self.__value_range = value_range
        if (sort in ('integer', 'float')) and (value_range is None):
            self.__value_range = (Distribution.DEFAULT_MIN_VALUE, Distribution.DEFAULT_MAX_VALUE)

    @property
    def sort(self):
        return self.__sort

    @property
    def value_range(self):
        """
        The distributions value range.
        :rtype: (Real, Real)
        """
        return self.__value_range

    def generateSample(self, evaluationContext, paramValues):
        """
        Generates a sample from this distribution.

        :param EvaluationContext evaluationContext: The current evaluation context.
        :param list|tuple paramValues: the parameter values
        """
        raise NotImplementedError()

    def describe(self):
        """
        Returns a short textual description of the distribution.
        :rtype: str
        """
        return "n/a"


class UniformDistribution(Distribution):
    def __init__(self, sort, value_range=None):
        Distribution.__init__(self, sort, value_range)

    def generateSample(self, evaluationContext, paramValues):
        if self.sort == 'integer':
            return np.random.randint(*self.value_range)
        elif self.sort == 'float':
            return np.random.uniform(*self.value_range)
        elif self.sort == 'boolean':
            return np.random.choice([True, False])
        else:
            # user defined sort      
            domain = evaluationContext.getDomain(self.sort)
            sample = np.random.choice(list(domain))
            return sample

    def describe(self):
        s = "U<{}>".format(self.sort)
        if self.value_range is not None:
            s += "({},{})".format(self.value_range[0], self.value_range[1])
        return s


class ArgumentIdentityDistribution(Distribution):
    """
    Creates a stub distribution that passes the specified argument through.
    """

    def __init__(self, sort, param_index):
        Distribution.__init__(self, sort, None)
        self.__param_index = param_index

    @property
    def param_index(self):
        return self.__param_index

    def generateSample(self, evaluation_context, param_values):
        return param_values[self.__param_index]

    def describe(self):
        return "id[{}]".format(self.__param_index)


class BernoulliDistribution(Distribution):
    def __init__(self, probability):
        Distribution.__init__(self, 'boolean')
        self.__probability = probability

    @property
    def probability(self):
        return self.__probability

    def generateSample(self, evaluationContext, paramValues):
        r = np.random.random()
        return r < self.__probability

    def describe(self):
        return "Pr({})".format(self.__probability)


class NormalDistribution(Distribution):
    def __init__(self, sort, mu, sigma):
        if sort not in ["float", "integer"]:
            raise SALMAException(
                "Trying to use normal distribution for sort %s but only integer or float allowed." % sort)
        super().__init__(sort, (float("-inf"), float("inf")))
        self.__mu = mu
        self.__sigma = sigma

    @property
    def mu(self):
        return self.__mu

    @property
    def sigma(self):
        return self.__sigma

    def generateSample(self, evaluationContext, paramValues):
        val = np.random.normal(self.__mu, self.__sigma)
        if self.sort == "integer":
            return round(val)
        else:
            return val

    def describe(self):
        return "N({:.4},{:.4})".format(self.mu, self.sigma ** 2)


class ExponentialDistribution(Distribution):

    def __init__(self, sort, rate):
        if sort not in ["float", "integer"]:
            raise SALMAException(
                "Trying to use exponential distribution for sort %s but only integer or float allowed." % sort)
        super().__init__(sort, (float("-inf"), float("inf")))
        self.__rate = rate

    def generateSample(self, evaluation_context, param_values):
        val = np.random.exponential(self.__rate)
        if self.sort == "integer":
            return round(val)
        else:
            return val

    @property
    def rate(self):
        return self.__rate

    def describe(self):
        return "Exp({:4})".format(self.rate)


class ConstantDistribution(Distribution):
    def __init__(self, sort, value):
        super().__init__(sort)
        self.__value = value

    @property
    def value(self):
        return self.__value

    def generateSample(self, evaluationContext, paramValues):
        return self.__value

    def describe(self):
        return "Constant({})".format(self.__value)


class OptionalDistribution(Distribution):

    def __init__(self, probability, nested_distribution):
        """
        :param float probability: the probability
        :param Distribution nested_distribution: the nested distribution
        """
        super().__init__(nested_distribution.sort, nested_distribution.value_range)
        #: :type: float
        self.__probability = probability
        #: :type: Distribution
        self.__nested_distribution = nested_distribution

    @property
    def probability(self) -> float:
        return self.__probability

    @property
    def nested_distribution(self) -> Distribution:
        return self.__nested_distribution

    def generateSample(self, evaluation_ontext, param_values):
        r = np.random.random()
        if r < self.__probability:
            return self.__nested_distribution.generateSample(evaluation_ontext, param_values)
        else:
            return None


class Never(Distribution):
    """
    A convience distribution meant for scheduled events that always returns None, which means that the event will
    not be scheduled.
    """
    def __init__(self):
        super().__init__("integer")

    def generateSample(self, ec, pv):
        return None

NEVER = Never()

DONT_OCCUR = ConstantDistribution("boolean", False)


class Zero(Distribution):
    """
    A convinience distribution that always returns 0.
    """

    def __init__(self):
        super().__init__("integer")

    def generateSample(self, ec, pv):
        return 0


ZERO_INT = Zero()

class Categorical(Distribution):
    """
    A categorical distribution that is specified by a collection of category-probability pairs.
    """

    def __init__(self, sort, categories):
        """
        Creates a categorical distribution with the given sort and the given categories with
        associated probabilities.

        :param str sort: the sort of the categories
        :param list[(obj, float)] categories: for each category a tuple of form (category, probability)
        """
        super().__init__(sort)
        self.__categories = categories
        if not 0.99 < sum(map(lambda x: x[1], categories)) < 1.01:
            raise SALMAException("Probabilities for categorical distribution don't add up to 1.")

    def generateSample(self, evaluationContext, paramValues):
        x = np.random.random()
        y = 0
        for cat in self.__categories:
            if y <= x < y + cat[1]:
                return cat[0]
            y += cat[1]
        raise SALMAException("Can't select category.")  # should never be reached because of check in constructor


class ComposedDistribution(Distribution):
    """
    A composed distribution where the parameters of the inner distribution are generated by
    the outer distribution.
    """

    def __init__(self, inner_dist_class, inner_sort, *param_dists, **kwargs):
        """
        :param type inner_dist_class: the inner distribution class
        :param list[Distribution] param_dists: the parameter distributions
        """
        super().__init__(inner_sort)
        self.__inner_dist_class = inner_dist_class
        self.__param_dists = param_dists
        self.__inner_sort = inner_sort
        self.__expects_sort = kwargs.get("expects_sort", False)

    def generateSample(self, evaluationContext, paramValues):
        params = []
        for pd in self.__param_dists:
            params.append(pd.generateSample(evaluationContext, paramValues))

        dist = (self.__inner_dist_class(self.__inner_sort, *params)
                if self.__expects_sort is True else self.__inner_dist_class(*params))
        return dist.generateSample(evaluationContext, paramValues)

class GeometricDistribution(Distribution):

    def __init__(self, p):
        super().__init__("integer")
        self.__p = p

    def generateSample(self, evaluationContext, paramValues):
        if self.__p is None:
            return None
        else:
            return np.random.geometric(self.__p)

class CustomDistribution(Distribution):

    def __init__(self, sort, source):
        super().__init__(sort)
        self.__source = source

    def generateSample(self, evaluationContext, paramValues):
        source_type = evaluationContext.determine_source_type(self.__source, paramValues)
        result = evaluationContext.evaluateFunction(source_type, self.__source, *paramValues)
        return result


