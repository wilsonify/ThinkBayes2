"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import thinkbayes
from thinkbayes import thinkplot


class Hyrax(thinkbayes.Suite):
    """Represents hypotheses about how many hyraxes there are."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: total population
        data: # tagged, # caught (n), # of caught who were tagged (k)
        """
        tagged, n, k = data
        if hypo < tagged + n - k:
            return 0

        p = tagged / hypo
        like = thinkbayes.EvalBinomialPmf(k, n, p)
        return like


class Hyrax2(thinkbayes.Suite):
    """Represents hypotheses about how many hyraxes there are."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: total population (N)
        data: # tagged (K), # caught (n), # of caught who were tagged (k)
        """
        n_total_population = hypo
        k_tagged, n_caught, k_caught_tagged = data

        if hypo < k_tagged + (n_caught - k_caught_tagged):
            return 0

        like = thinkbayes.EvalHypergeomPmf(k_caught_tagged, n_total_population, k_tagged, n_caught)
        return like


def main():
    hypos = range(1, 1000)
    suite = Hyrax(hypos)
    suite2 = Hyrax2(hypos)

    data = 10, 10, 2
    suite.update(data)
    suite2.update(data)

    thinkplot.plot_pdf_line(suite, label="binomial")
    thinkplot.plot_pdf_line(suite, label="hypergeom")
    thinkplot.show_plot()


if __name__ == "__main__":
    main()
