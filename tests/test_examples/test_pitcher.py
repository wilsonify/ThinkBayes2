"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Suite

import thinkplot


class Subclass(Suite):
    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.
        
        data: 
        hypo: 
        """
        like = 1
        return like


def test_subclass():
    prior = Subclass([1, 2, 3])
    posterior = prior.Copy()
    posterior.Update(1)
