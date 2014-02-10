import random
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
        :param list paramValues: the parameter values
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
            return random.randint(*(self.value_range))
        elif self.sort == 'float':
            return random.uniform(*(self.value_range))
        elif self.sort == 'boolean':
            return random.choice([True, False])
        else:
            # user defined sort      
            domain = evaluationContext.getDomain(self.sort)
            sample = random.choice(list(domain))
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
        r = random.uniform(0, 1)
        return r <= self.__probability

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
        val = random.normalvariate(self.__mu, self.__sigma)
        if self.sort == "integer":
            return round(val)
        else:
            return val

    def describe(self):
        return "N({:.4},{:.4})".format(self.mu, self.sigma ** 2)
