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
        if self.sort is 'integer':
            return random.randint(*(self.valueRange))
        elif self.sort is 'float':
            return random.uniform(*(self.valueRange))
        elif self.sort is 'boolean':
            return random.choice([True, False])
        else:
            # user defined sort      
            domain = evaluationContext.getDomain(self.sort)
            sample = random.choice(list(domain))
            return sample
        
  
  
class ArgumentIdentityDistribution(Distribution):
    '''
    passes the specified argument through
    '''
    def __init__(self, paramIndex):
        Distribution.__init__(self, None, None)
        self.__paramIndex = paramIndex

    @property
    def paramIndex(self):
        return self.__paramIndex

    
    def generateSample(self, evaluationContext, paramValues):
        return paramValues[self.__paramIndex]
    
     
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
        