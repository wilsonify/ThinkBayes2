"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import thinkplot
from thinkbayes.scripts.dice import Dice


class Train(Dice):
    """Represents hypotheses about how many trains the company has.

    The likelihood function for the train problem is the same as
    for the Dice problem.
    """

    def Likelihood(self, data, hypo):
        sample_size = hypo
        k, m = data
        if sample_size < m:
            return 0

        return m ** (k - 1) / sample_size ** k


def main():
    hypos = range(1, 1001)
    suite = Train(hypos)

    data = 3, 70
    suite.Update(data)
    print("posterior mean", suite.Mean())

    thinkplot.plot_pmf_line(suite, label="after 70")
    thinkplot.show_plot(xlabel="Number of trains", ylabel="PMF")


if __name__ == "__main__":
    main()
