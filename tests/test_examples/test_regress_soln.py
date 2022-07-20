"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
from itertools import product

import numpy as np
from scipy.stats import norm

import thinkplot
from thinkbayes import Suite, Joint


class Regress1(Suite, Joint):
    def Likelihood(self, data, hypo):
        """

        data: x, y
        hypo: slope, inter, sigma
        """
        return 1


# Solution

class Regress2(Suite, Joint):
    def Likelihood(self, data, hypo):
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



    # ### Grid algorithm
    #
    # We can solve the problem first using a grid algorithm, with uniform priors for slope, intercept, and sigma.
    #
    # As an exercise, fill in this likelihood function, then test it using the code below.
    #
    # Your results will depend on the random data you generated, but in general you should find that the posterior marginal distributions peak near the actual parameters.

    params = np.linspace(-4, 4, 21)

    sigmas = np.linspace(0.1, 2, 20)

    hypos = product(params, params, sigmas)

    suite = Regress2(hypos)

    for data in zip(xs, ys):
        suite.Update(data)
