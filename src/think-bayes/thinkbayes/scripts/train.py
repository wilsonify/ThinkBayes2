"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes.scripts.dice import Dice

from thinkbayes import thinkplot


class Train(Dice):
    """Represents hypotheses about how many trains the company has.

    The likelihood function for the train problem is the same as
    for the Dice problem.
    """


def main():
    hypos = range(1, 1001)
    suite = Train(hypos)

    suite.Update(60)
    print(suite.Mean())

    thinkplot.pre_plot(1)
    thinkplot.plot_pmf_line(suite)
    thinkplot.save_plot(
        root="train1",
        xlabel="Number of trains",
        ylabel="Probability",
        formats=["pdf", "eps"],
    )


if __name__ == "__main__":
    main()
