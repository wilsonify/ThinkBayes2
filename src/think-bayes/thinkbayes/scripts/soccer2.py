"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy

import thinkbayes
from thinkbayes import thinkplot


class Soccer(thinkbayes.Suite):
    """Represents hypotheses about."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: goal rate in goals per game
        data: number of goals scored in a game
        """
        # TODO: fill this in
        like = 1
        return like

    @staticmethod
    def predictive_dist(label="pred"):
        """Computes the distribution of goals scored in a game.

        returns: new Pmf (mixture of Poissons)
        """
        logging.debug("%r", f"label={label}")
        # TODO: fill this in
        lam = 1
        pred = thinkbayes.MakePoissonPmf(lam, 15)
        return pred


def main():
    hypos = numpy.linspace(0, 12, 201)

    # start with a prior based on a pseudo observation
    # chosen to yield the right prior mean
    suite1 = Soccer(hypos, label="Germany")
    suite1.update(0.34)
    suite2 = suite1.Copy(label="Argentina")

    # update with the results of World Cup 2014 final
    suite1.update(1)
    suite2.update(0)

    print("posterior mean Germany", suite1.Mean())
    print("posterior mean Argentina", suite2.Mean())

    # plot the posteriors
    thinkplot.pre_plot(2)
    thinkplot.plot_pdfs([suite1, suite2])
    thinkplot.show_plot()

    # TODO: compute posterior prob Germany is better than Argentina

    # TODO: compute the Bayes factor of the evidence

    # compute predictive distributions for goals scored in a rematch
    pred1 = suite1.predictive_dist(label="Germany")
    pred2 = suite2.predictive_dist(label="Argentina")

    # plot the predictive distributions
    thinkplot.pre_plot(2)
    thinkplot.plot_pdfs([pred1, pred2])
    thinkplot.show_plot()

    # TODO: compute predictive probability of winning rematch


if __name__ == "__main__":
    main()
