"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging
import random

import thinkbayes
from thinkbayes import thinkplot

FORMATS = ["pdf", "eps", "png"]


class Die(thinkbayes.Pmf):
    """Represents the PMF of outcomes for a die."""

    def __init__(self, sides, label=""):
        """Initializes the die.

        sides: int number of sides
        label: string
        """
        hypos = range(1, sides + 1)
        thinkbayes.Pmf.__init__(self, hypos, label=label)


def pmf_max(pmf1, pmf2):
    """Computes the distribution of the max of values drawn from two Pmfs.

    pmf1, pmf2: Pmf objects

    returns: new Pmf
    """
    res = thinkbayes.Pmf()
    for v1, p1 in pmf1.items():
        for v2, p2 in pmf2.items():
            res.incr(max(v1, v2), p1 * p2)
    return res


def main():
    pmf_dice = thinkbayes.Pmf()
    pmf_dice.set(Die(4), 5)
    pmf_dice.set(Die(6), 4)
    pmf_dice.set(Die(8), 3)
    pmf_dice.set(Die(12), 2)
    pmf_dice.set(Die(20), 1)
    pmf_dice.normalize()

    mix = thinkbayes.Pmf()
    for die, weight in pmf_dice.items():
        for outcome, prob in die.items():
            mix.incr(outcome, weight * prob)

    mix = thinkbayes.make_mixture(pmf_dice)

    thinkplot.plot_hist_bar(mix, width=0.9)
    thinkplot.save_plot(
        root="dungeons3", xlabel="Outcome", ylabel="Probability", formats=FORMATS
    )

    random.seed(17)

    d6 = Die(6, "d6")

    dice = [d6] * 3
    three = thinkbayes.sample_sum(dice, 1000)
    three.label = "sample"
    three.print_size()

    three_exact = d6 + d6 + d6
    three_exact.label = "exact"
    three_exact.print_size()

    thinkplot.pre_plot(num=2)
    thinkplot.plot_pmf_line(three)
    thinkplot.plot_pmf_line(three_exact, linestyle="dashed")
    thinkplot.save_plot(
        root="dungeons1",
        xlabel="Sum of three d6",
        ylabel="Probability",
        axis=[2, 19, 0, 0.15],
        formats=FORMATS,
    )

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=1)

    # compute the distribution of the best attribute the hard way
    best_attr2 = pmf_max(three_exact, three_exact)
    best_attr4 = pmf_max(best_attr2, best_attr2)
    best_attr6 = pmf_max(best_attr4, best_attr2)
    logging.debug("%r", f"best_attr6={best_attr6}")
    # thinkplot.Pmf(best_attr6)

    # and the easy way
    best_attr_cdf = three_exact.max(6)
    best_attr_cdf.label = ""
    best_attr_pmf = best_attr_cdf.make_pmf()
    best_attr_pmf.print_size()

    thinkplot.plot_pmf_line(best_attr_pmf)
    thinkplot.save_plot(
        root="dungeons2",
        xlabel="Best of three d6",
        ylabel="Probability",
        axis=[2, 19, 0, 0.23],
        formats=FORMATS,
        legend=False,
    )


if __name__ == "__main__":
    main()
