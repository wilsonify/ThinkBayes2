"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Suite

from thinkbayes import thinkplot


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
    thinkplot.Hist(prior)
    thinkplot.Config(xlabel="x", ylabel="PMF")

    posterior = prior.Copy()
    posterior.Update(1)
    thinkplot.Hist(prior, color="gray")
    thinkplot.Hist(posterior)
    thinkplot.Config(xlabel="x", ylabel="PMF")
