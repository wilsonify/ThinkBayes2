"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes.scripts.dice import Dice

import thinkbayes
from thinkbayes import thinkplot


class Train(Dice):
    """Represents hypotheses about how many trains the company has."""


class Train2(Dice):
    """Represents hypotheses about how many trains the company has."""

    def __init__(self, hypos, alpha=1.0):
        """Initializes the hypotheses with a power law distribution.

        hypos: sequence of hypotheses
        alpha: parameter of the power law prior
        """
        thinkbayes.Pmf.__init__(self)
        for hypo in hypos:
            self.Set(hypo, hypo ** (-alpha))
        self.Normalize()


def make_posterior(high, dataset, constructor):
    """Makes and updates a Suite.

    high: upper bound on the range of hypotheses
    dataset: observed data to use for the update
    constructor: function that makes a new suite

    Returns: posterior Suite
    """
    hypos = range(1, high + 1)
    suite = constructor(hypos)
    suite.name = str(high)

    for data in dataset:
        suite.Update(data)

    return suite


def compare_priors():
    """Runs the analysis with two different priors and compares them."""
    dataset = [60]
    high = 1000

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)

    constructors = [Train, Train2]
    labels = ["uniform", "power law"]

    for constructor, label in zip(constructors, labels):
        suite = make_posterior(high, dataset, constructor)
        suite.name = label
        thinkplot.plot_pmf_line(suite)

    thinkplot.save_plot(root="train4", xlabel="Number of trains", ylabel="Probability")


def main():
    compare_priors()

    dataset = [30, 60, 90]

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=3)

    for high in [500, 1000, 2000]:
        suite = make_posterior(high, dataset, Train2)
        print(high, suite.Mean())

    thinkplot.save_plot(root="train3", xlabel="Number of trains", ylabel="Probability")

    interval = suite.Percentile(5), suite.Percentile(95)
    print(interval)

    cdf = thinkbayes.Cdf(suite)
    interval = cdf.Percentile(5), cdf.Percentile(95)
    print(interval)


if __name__ == "__main__":
    main()
