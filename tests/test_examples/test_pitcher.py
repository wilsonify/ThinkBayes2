"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Suite

from thinkbayes import thinkplot


class Subclass(Suite):
    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.
        
        data: 
        hypo: 
        """
        like = 1
        return like


def test_subclass():
    prior = Subclass([1, 2, 3])
    thinkplot.plot_hist_bar(prior)
    thinkplot.config_plot(xlabel="x", ylabel="PMF")

    posterior = prior.copy()
    posterior.update(1)
    thinkplot.plot_hist_bar(prior, color="gray")
    thinkplot.plot_hist_bar(posterior)
    thinkplot.config_plot(xlabel="x", ylabel="PMF")
