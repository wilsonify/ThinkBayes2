"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes.scripts.dice import Dice

from thinkbayes import thinkplot


class Train(Dice):
    """The likelihood function for the train problem is the same as
    for the Dice problem."""


def compute_mean(suite):
    total = 0
    for hypo, prob in suite.Items():
        total += hypo * prob
    return total


def make_posterior(high, dataset):
    hypos = range(1, high + 1)
    suite = Train(hypos)
    suite.name = str(high)

    for data in dataset:
        suite.Update(data)

    thinkplot.plot_pmf_line(suite)
    return suite


def main():
    dataset = [30, 60, 90]

    for high in [500, 1000, 2000]:
        suite = make_posterior(high, dataset)
        print(high, suite.Mean())

    thinkplot.save_plot(root="train2", xlabel="Number of trains", ylabel="Probability")


if __name__ == "__main__":
    main()
