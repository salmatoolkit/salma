"""
Created on 04.09.2013

@author: christian
"""

import numpy as np


class HypothesisTest(object):
    """
    Base clas for hypothesis tests. Offers the method check_hypothesis_accepted.
    """

    def check_hypothesis_accepted(self, m, numberOfDefects):
        """
        If one of the hypothesis is accepted, return its index (usually 0 or 1). Otherwise return None
         which means that testing should continue.
        """
        raise NotImplementedError()


class SequentialProbabilityRatioTest(HypothesisTest):
    """
    An implementation of the sequential hypothesis test proposed by A. Wald.
    Tests the hypothesis that the probability of failure for an experiment is
    at most a specified marginal probability p'.

    Instead of testing the simple hypothesis p = p' against p > p', a tolerance region
    p0 < p' < p1 is defined.

    """

    def __init__(self, p0, p1, alpha, beta):
        '''
        p0: accept hypothesis if actual probability of defect is < p0
        p1: reject hypothesis if actual probability of defect is > p1
        alpha: max probability of error of first type (reject if p <= p0)
        beta: max probability of error of second type (accept if p >= p0)
        '''
        self.__p0 = p0
        self.__p1 = p1
        self.__alpha = alpha
        self.__beta = beta
        
        denominator = (np.log(p1 / p0) - 
                        np.log((1 - p1) / (1 - p0)))
        
        self.__base_A = np.log( beta / (1 - alpha) ) / denominator
        self.__m_factor = np.log( (1 - p0) / (1 - p1)  ) / denominator
        
        self.__base_R = np.log( (1 - beta) / alpha ) / denominator
        
    def check_hypothesis_accepted(self, m, numberOfDefects):
        A_m = self.__base_A + self.__m_factor * m
        R_m = self.__base_R + self.__m_factor * m
        
        if numberOfDefects <= A_m:
            return 0
        elif numberOfDefects >= R_m:
            return 1
        else:
            return None
         
    
    
    
        
    
        