"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from dice import Dice

from src import thinkplot


class Train(Dice):
    """Represents hypotheses about how many trains the company has.

    The likelihood function for the train problem is the same as
    for the Dice problem.
    """

    def Likelihood(self, data, hypo):
        N = hypo
        k, m = data
        if N < m:
            return 0

        return m ** (k - 1) / N ** k


def main():
    hypos = range(1, 1001)
    suite = Train(hypos)

    data = 3, 70
    suite.Update(data)
    print("posterior mean", suite.Mean())

    thinkplot.Pmf(suite, label="after 70")
    thinkplot.Show(xlabel="Number of trains", ylabel="PMF")


if __name__ == "__main__":
    main()
