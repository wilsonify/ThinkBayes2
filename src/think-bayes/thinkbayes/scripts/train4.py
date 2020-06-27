"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import thinkplot
from thinkbayes.scripts.dice import Dice
NTRAINS_LABEL = "Number of trains"

class Train(Dice):
    """Represents hypotheses about how many trains the company has.

    The likelihood function for the train problem is the same as
    for the Dice problem.
    """


def main():
    hypos = range(1, 1001)
    suite = Train(hypos)

    for data in [32, 54, 70]:
        suite.update(data)
        thinkplot.plot_pmf_line(suite, label=f"after {data}")

    thinkplot.show_plot(xlabel=NTRAINS_LABEL, ylabel="PMF")

    print("posterior mean", suite.mean())


if __name__ == "__main__":
    main()
