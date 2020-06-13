"""
Think Bayes
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy as np
from thinkbayes import Pmf, Suite, Joint
import thinkbayes
from thinkbayes import thinkplot


class Battleship(Suite, Joint):
    lam = 1

    def Likelihood(self, hypo, data):
        x_actual, y_actual = hypo
        x_guess, y_guess, result = data
        d = np.hypot(x_guess - x_actual, y_guess - y_actual)
        p_hit = np.exp(-self.lam * d)
        return p if result == "hit" else 1 - p


def test_battle():
    gap = thinkbayes.MakeNormalPmf(7, 1, 3)
    thinkplot.plot(gap)

    metapmf = thinkbayes.Pmf()
    for t, p in gap.Items():
        arrivals = thinkbayes.MakePoissonPmf(1.3 * t, 25)
        thinkplot.plot(arrivals, color="C0", linewidth=0.1)
        metapmf[arrivals] = p

    metapmf = thinkbayes.Pmf()
    for t, p in gap.Items():
        arrivals = thinkbayes.MakePoissonPmf(1.3 * t, 25)
        thinkplot.plot(arrivals, color="C0", linewidth=0.1)
        metapmf[arrivals] = p

    mix = thinkbayes.MakeMixture(metapmf)
    mix.Mean()
    thinkplot.Hist(mix)
    thinkplot.decorate(xlabel="Number of passengers", ylabel="PMF")

    mix[10]
