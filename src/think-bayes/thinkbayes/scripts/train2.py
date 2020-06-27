"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes.scripts.dice import Dice

from thinkbayes import thinkplot
NTRAINS_LABEL = "Number of trains"

class Train(Dice):
    """The likelihood function for the train problem is the same as
    for the Dice problem."""


def compute_mean(suite):
    total = 0
    for hypo, prob in suite.items():
        total += hypo * prob
    return total


def make_posterior(high, dataset):
    hypos = range(1, high + 1)
    suite = Train(hypos)
    suite.name = str(high)

    for data in dataset:
        suite.update(data)

    thinkplot.plot_pmf_line(suite)
    return suite


def main():
    dataset = [30, 60, 90]

    for high in [500, 1000, 2000]:
        suite = make_posterior(high, dataset)
        print(high, suite.mean())

    thinkplot.save_plot(root="train2", xlabel=NTRAINS_LABEL, ylabel="Probability")


if __name__ == "__main__":
    main()
