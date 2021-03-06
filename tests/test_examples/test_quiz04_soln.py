"""
Think Bayes
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np
import thinkbayes
from thinkbayes import Suite, Joint
from thinkbayes import thinkplot


class Battleship(Suite, Joint):
    lam = 1

    def likelihood(self, hypo, data):
        x_actual, y_actual = hypo
        x_guess, y_guess, result = data
        d = np.hypot(x_guess - x_actual, y_guess - y_actual)
        p_hit = np.exp(-self.lam * d)
        return p_hit if result == "hit" else 1 - p_hit


def test_battle():
    gap = thinkbayes.make_normal_pmf(7, 1, 3)
    thinkplot.plot(gap)

    metapmf = thinkbayes.Pmf()
    for t, p in gap.items():
        arrivals = thinkbayes.make_poisson_pmf(1.3 * t, 25)
        thinkplot.plot(arrivals, color="C0", linewidth=0.1)
        metapmf[arrivals] = p

    metapmf = thinkbayes.Pmf()
    for t, p in gap.items():
        arrivals = thinkbayes.make_poisson_pmf(1.3 * t, 25)
        thinkplot.plot(arrivals, color="C0", linewidth=0.1)
        metapmf[arrivals] = p

    mix = thinkbayes.make_mixture(metapmf)
    mix.mean()
    thinkplot.plot_hist_bar(mix)
    thinkplot.decorate(xlabel="Number of passengers", ylabel="PMF")

    logging.info("%r", f"likelihood = {mix[10]}")

