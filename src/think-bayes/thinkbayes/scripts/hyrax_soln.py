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
        N = hypo
        K, n, k = data

        if hypo < K + (n - k):
            return 0

        like = thinkbayes.EvalHypergeomPmf(k, N, K, n)
        return like


def main():
    hypos = range(1, 1000)
    suite = Hyrax(hypos)
    suite2 = Hyrax2(hypos)

    data = 10, 10, 2
    suite.Update(data)
    suite2.Update(data)

    thinkplot.plot_pdf_line(suite, label="binomial")
    thinkplot.plot_pdf_line(suite, label="hypergeom")
    thinkplot.show_plot()


if __name__ == "__main__":
    main()
