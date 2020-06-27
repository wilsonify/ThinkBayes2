"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np

from thinkbayes import Suite, Joint

from thinkbayes import thinkplot
from thinkbayes.thinkplot import POSTERIOR_MARGINAL_LABEL


def test_bayes_reg():
    # ## Bayesian regression
    #
    # This notebook presents a simple example of Bayesian regression using sythetic data
    #
    # ### Data
    #
    # Suppose there is a linear relationship between `x` and `y` with slope 2 and intercept 1, but the measurements of `y` are noisy; specifically, the noise is Gaussian with mean 0 and `sigma = 0.3`.

    slope = 2
    inter = 1
    sigma = 0.3

    xs = np.linspace(0, 1, 6)

    ys = inter + slope * xs + np.random.normal(0, sigma, len(xs))

    thinkplot.plot(xs, ys)
    thinkplot.decorate(xlabel="x", ylabel="y")

    # ### Grid algorithm
    #
    # We can solve the problem first using a grid algorithm, with uniform priors for slope, intercept, and sigma.
    #
    # As an exercise, fill in this likelihood function, then test it using the code below.
    #
    # Your results will depend on the random data you generated, but in general you should find that the posterior marginal distributions peak near the actual parameters.

    class Regress(Suite, Joint):
        def likelihood(self, data, hypo):
            """

            data: x, y
            hypo: slope, inter, sigma
            """
            return 1

    # Solution

    from scipy.stats import norm

    class Regress(Suite, Joint):
        def likelihood(self, data, hypo):
            """

            data: x, y
            hypo: slope, inter, sigma
            """
            x, y = data
            slope, inter, sigma = hypo

            yfit = inter + slope * x
            error = yfit - y
            like = norm(0, sigma).pdf(error)
            return like

    params = np.linspace(-4, 4, 21)

    sigmas = np.linspace(0.1, 2, 20)

    from itertools import product

    hypos = product(params, params, sigmas)

    suite = Regress(hypos)

    for data in zip(xs, ys):
        suite.update(data)

    thinkplot.plot_pdf_line(suite.marginal(0))
    thinkplot.decorate(
        xlabel="Slope", ylabel="PMF", title=POSTERIOR_MARGINAL_LABEL
    )

    thinkplot.plot_pdf_line(suite.marginal(1))
    thinkplot.decorate(
        xlabel="Intercept", ylabel="PMF", title=POSTERIOR_MARGINAL_LABEL
    )

    thinkplot.plot_pdf_line(suite.marginal(2))
    thinkplot.decorate(
        xlabel="Sigma", ylabel="PMF", title=POSTERIOR_MARGINAL_LABEL
    )

    # ### MCMC
    #
    # Implement this model using MCMC.  As a starting place, you can use this example from [Computational Statistics in Python](http://people.duke.edu/~ccc14/sta-663-2016/16C_PyMC3.html#Linear-regression).
    #
    # You also have the option of using the GLM module, [described here](https://docs.pymc.io/notebooks/GLM-linear.html).

    import pymc3 as pm

    logging.info("%r", f"pm.GLM = {pm.GLM}")


    thinkplot.plot(xs, ys)
    thinkplot.decorate(xlabel="x", ylabel="y")

    import pymc3 as pm

    with pm.Model() as model:
        """Fill this in"""

    # Solution

    with pm.Model() as model:
        slope = pm.Uniform("slope", -4, 4)
        inter = pm.Uniform("inter", -4, 4)
        sigma = pm.Uniform("sigma", 0, 2)

        y_est = slope * xs + inter
        y = pm.Normal("y", mu=y_est, sd=sigma, observed=ys)
        trace = pm.sample_prior_predictive(100)

    # Solution

    for y_prior in trace["y"]:
        thinkplot.plot(xs, y_prior, color="gray", linewidth=0.5)

    thinkplot.decorate(xlabel="x", ylabel="y")

    # Solution

    with pm.Model() as model:
        slope = pm.Uniform("slope", -4, 4)
        inter = pm.Uniform("inter", -4, 4)
        sigma = pm.Uniform("sigma", 0, 2)

        y_est = slope * xs + inter
        y = pm.Normal("y", mu=y_est, sd=sigma, observed=ys)
        trace = pm.sample(1000, tune=2000)

    # Solution

    pm.traceplot(trace)

    # The posterior distributions for these parameters should be similar to what we got with the grid algorithm.
