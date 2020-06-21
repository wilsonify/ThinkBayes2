"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy

import thinkbayes
from thinkbayes import thinkplot


class Soccer(thinkbayes.Suite):
    """Represents hypotheses about."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: goal rate in goals per game
        data: interarrival time in minutes
        """
        x = data
        lam = hypo / 90
        like = thinkbayes.EvalExponentialPdf(x, lam)
        return like

    def PredRemaining(self, rem_time, score):
        """Plots the predictive distribution for final number of goals.

        rem_time: remaining time in the game in minutes
        score: number of goals already scored
        """
        metapmf = thinkbayes.Pmf()
        for lam, prob in self.Items():
            lt = lam * rem_time / 90
            pred = thinkbayes.MakePoissonPmf(lt, 15)
            metapmf[pred] = prob
            # thinkplot.Pdf(pred, color='gray', alpha=0.1, linewidth=0.5)

        mix = thinkbayes.MakeMixture(metapmf)
        mix += score
        thinkplot.plot_hist_bar(mix)
        thinkplot.show_plot()


def main():
    hypos = numpy.linspace(0, 12, 201)
    suite = Soccer(hypos)

    # the mean number of goals per game was 2.67
    mean_rate = 2.67 / 2
    mean_interarrival = 90 / mean_rate

    # start with a prior based on the mean interarrival time
    suite.update(mean_interarrival)
    thinkplot.plot_pdf_line(suite, label="prior")
    print("prior mean", suite.Mean())

    suite.update(11)
    thinkplot.plot_pdf_line(suite, label="posterior 1")
    print("after one goal", suite.Mean())

    suite.update(12)
    thinkplot.plot_pdf_line(suite, label="posterior 2")
    print("after two goals", suite.Mean())

    thinkplot.show_plot()

    # plot the predictive distribution
    suite.PredRemaining(90 - 23, 2)


if __name__ == "__main__":
    main()
