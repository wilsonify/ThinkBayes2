


# # Think Bayes
#
# Copyright 2018 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT




import numpy as np


from thinkbayes import Pmf, Cdf, Suite, Joint

from thinkbayes import thinkplot

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


from scipy.stats import norm


class Regress(Suite, Joint):
    def Likelihood(self, data, hypo):
        """
        
        data: x, y
        hypo: slope, inter, sigma
        """
        return 1


# Solution goes here


params = np.linspace(-4, 4, 21)

sigmas = np.linspace(0.1, 2, 20)

from itertools import product

hypos = product(params, params, sigmas)

suite = Regress(hypos)

for data in zip(xs, ys):
    suite.Update(data)

thinkplot.Pdf(suite.Marginal(0))
thinkplot.decorate(
    xlabel="Slope", ylabel="PMF", title="Posterior marginal distribution"
)

thinkplot.Pdf(suite.Marginal(1))
thinkplot.decorate(
    xlabel="Intercept", ylabel="PMF", title="Posterior marginal distribution"
)

thinkplot.Pdf(suite.Marginal(2))
thinkplot.decorate(
    xlabel="Sigma", ylabel="PMF", title="Posterior marginal distribution"
)

# ### MCMC
#
# Implement this model using MCMC.  As a starting place, you can use this example from [Computational Statistics in Python](http://people.duke.edu/~ccc14/sta-663-2016/16C_PyMC3.html#Linear-regression).
#
# You also have the option of using the GLM module, [described here](https://docs.pymc.io/notebooks/GLM-linear.html).

import pymc3 as pm

pm.GLM

thinkplot.plot(xs, ys)
thinkplot.decorate(xlabel="x", ylabel="y")

import pymc3 as pm

with pm.Model() as model:
    """Fill this in"""

# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# The posterior distributions for these parameters should be similar to what we got with the grid algorithm.
