'''
Created on 26.07.2013

@author: kroiss
'''


import random
from salma.SMCException import SMCException
from salma.model.evaluationcontext import EvaluationContext

class Distribution(object):
    def __init__(self, sort, valueRange = None):
        self.__sort = sort
        if (sort in ('integer', 'float')) and (valueRange == None):
            raise(SMCException(
                    "No value range specified for uniform distribution of sort {}".format(sort)))
         
        self.__valueRange = valueRange
    
    @property
    def sort(self):
        return self.__sort
    
    @property
    def valueRange(self):
        return self.__valueRange
    
    def generateSample(self, evaluationContext, paramValues):
        '''
        :param evaluationContext: EvaluationContext
        :param paramValues: list 
        '''
        raise NotImplementedError()
    

class UniformDistribution(Distribution):
    def __init__(self, sort, valueRange = None):
        Distribution.__init__(self, sort, valueRange)
         
    def generateSample(self, evaluationContext, paramValues):
        if self.sort == 'integer':
            return random.randint(*(self.valueRange))
        elif self.sort == 'float':
            return random.uniform(*(self.valueRange))
        elif self.sort == 'boolean':
            return random.choice([True, False])
        else:
            # user defined sort      
            domain = evaluationContext.getDomain(self.sort)
            sample = random.choice(list(domain))
            return sample
        
  
  
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
    
     
class BernoulliDistribution(Distribution):
    def __init__(self, probability):
        Distribution.__init__(self, 'boolean')
        self.__probability = probability
        
    @property
    def probability(self):
        return self.__probability
    
    def generateSample(self, evaluationContext, paramValues):
        r = random.uniform(0,1)
        return r <= self.__probability


class NormalDistribution(Distribution):
    def __init__(self, sort, mu, sigma):
        if sort not in ["float", "integer"]:
            raise SMCException("Trying to use normal distribution for sort %s but only integer or float allowed." % sort)
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





