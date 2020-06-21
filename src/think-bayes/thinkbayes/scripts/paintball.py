"""This file contains code used in "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging
import math
import sys

import thinkbayes
from thinkbayes import thinkplot

FORMATS = ["pdf", "eps", "png"]


def strafing_speed(alpha, beta, x):
    """Computes strafing speed, given location of shooter and impact.

    alpha: x location of shooter
    beta: y location of shooter
    x: location of impact

    Returns: derivative of x with respect to theta
    """
    theta = math.atan2(x - alpha, beta)
    speed = beta / math.cos(theta) ** 2
    return speed


def make_location_pmf(alpha, beta, locations):
    """Computes the Pmf of the locations, given alpha and beta. 

    Given that the shooter is at coordinates (alpha, beta),
    the probability of hitting any spot is inversely proportionate
    to the strafe speed.

    alpha: x position
    beta: y position
    locations: x locations where the pmf is evaluated

    Returns: Pmf object
    """
    pmf = thinkbayes.Pmf()
    for x in locations:
        prob = 1.0 / strafing_speed(alpha, beta, x)
        pmf.set(x, prob)
    pmf.normalize()
    return pmf


class Paintball(thinkbayes.Suite, thinkbayes.Joint):
    """Represents hypotheses about the location of an opponent."""

    def __init__(self, alphas, betas, locations):
        """Makes a joint suite of parameters alpha and beta.

        Enumerates all pairs of alpha and beta.
        Stores locations for use in Likelihood.

        alphas: possible values for alpha
        betas: possible values for beta
        locations: possible locations along the wall
        """
        self.locations = locations
        pairs = [(alpha, beta) for alpha in alphas for beta in betas]
        thinkbayes.Suite.__init__(self, pairs)

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: pair of alpha, beta
        data: location of a hit

        Returns: float likelihood
        """
        alpha, beta = hypo
        x = data
        pmf = make_location_pmf(alpha, beta, self.locations)
        like = pmf.prob(x)
        return like


def make_pmf_plot(alpha=10):
    """Plots Pmf of location for a range of betas."""
    locations = range(0, 31)

    betas = [10, 20, 40]
    thinkplot.pre_plot(num=len(betas))

    for beta in betas:
        pmf = make_location_pmf(alpha, beta, locations)
        pmf.name = f"beta = {beta}"
        thinkplot.plot_pdf_line(pmf)

    thinkplot.save_plot("paintball1", xlabel="Distance", ylabel="Prob", formats=FORMATS)


def make_posterior_plot(suite):
    """Plots the posterior marginal distributions for alpha and beta.

    suite: posterior joint distribution of location
    """
    marginal_alpha = suite.marginal(0)
    marginal_alpha.name = "alpha"
    marginal_beta = suite.marginal(1)
    marginal_beta.name = "beta"

    print("alpha CI", marginal_alpha.credible_interval(50))
    print("beta CI", marginal_beta.credible_interval(50))

    thinkplot.pre_plot(num=2)

    # thinkplot.Pmf(marginal_alpha)
    # thinkplot.Pmf(marginal_beta)

    thinkplot.plot_cdf_line(thinkbayes.make_cdf_from_pmf(marginal_alpha))
    thinkplot.plot_cdf_line(thinkbayes.make_cdf_from_pmf(marginal_beta))

    thinkplot.save_plot(
        "paintball2", xlabel="Distance", ylabel="Prob", loc=4, formats=FORMATS
    )


def make_conditional_plot(suite):
    """Plots marginal CDFs for alpha conditioned on beta.

    suite: posterior joint distribution of location
    """
    betas = [10, 20, 40]
    thinkplot.pre_plot(num=len(betas))

    for beta in betas:
        cond = suite.conditional(0, 1, beta)
        cond.name = f"beta = {beta}"
        thinkplot.plot_pdf_line(cond)

    thinkplot.save_plot("paintball3", xlabel="Distance", ylabel="Prob", formats=FORMATS)


def make_contour_plot(suite):
    """Plots the posterior joint distribution as a contour plot.

    suite: posterior joint distribution of location
    """
    thinkplot.contour_plot(suite.d, contour_bool=False, pcolor_bool=True)

    thinkplot.save_plot(
        "paintball4",
        xlabel="alpha",
        ylabel="beta",
        axis=[0, 30, 0, 20],
        formats=FORMATS,
    )


def make_credible_plot(suite):
    """Makes a plot showing several two-dimensional credible intervals.

    suite: Suite
    """
    d = dict((pair, 0) for pair in suite.values())

    percentages = [75, 50, 25]
    for p in percentages:
        interval = suite.max_like_interval(p)
        for pair in interval:
            d[pair] += 1

    thinkplot.contour_plot(d, contour_bool=False, pcolor_bool=True)
    thinkplot.annotate_figure(17, 4, "25", color="white")
    thinkplot.annotate_figure(17, 15, "50", color="white")
    thinkplot.annotate_figure(17, 30, "75")

    thinkplot.save_plot(
        "paintball5", xlabel="alpha", ylabel="beta", formats=FORMATS, legend=False
    )


def main(script):
    logging.debug("%r", f"script={script}")
    alphas = range(0, 31)
    betas = range(1, 51)
    locations = range(0, 31)

    suite = Paintball(alphas, betas, locations)
    suite.update_set([15, 16, 18, 21])

    make_credible_plot(suite)

    make_contour_plot(suite)

    make_posterior_plot(suite)

    make_conditional_plot(suite)

    make_pmf_plot()


if __name__ == "__main__":
    main(*sys.argv)
