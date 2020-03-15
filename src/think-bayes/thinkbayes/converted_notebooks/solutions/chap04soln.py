# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.4.0
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
# ---

# # Think Bayes solutions: Chapter 4
#
# This notebook presents solutions to exercises in Think Bayes.
#
# Copyright 2016 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT

# +
from __future__ import print_function, division


import warnings

warnings.filterwarnings("ignore")

import numpy as np

from thinkbayes import Pmf, Cdf, Suite
from thinkbayes import thinkplot


# -

# ## The Euro problem
#
# Here's a class that represents hypotheses about the probability a coin lands heads.


class Euro(Suite):
    def Likelihood(self, data, hypo):
        """Computes the likelihood of `data` given `hypo`.
        
        data: string 'H' or 'T'
        hypo: probability of heads, 0-100
        
        returns: float
        """
        x = hypo
        if data == "H":
            return x / 100
        else:
            return 1 - x / 100


# We can make a uniform prior and update it with 140 heads and 110 tails:

# +
suite = Euro(range(0, 101))
dataset = "H" * 140 + "T" * 110

for data in dataset:
    suite.Update(data)
# -

# And here's what the posterior looks like.

thinkplot.Pdf(suite)

# We can summarize the posterior several ways, including the mean:

suite.Mean()

# Median:

suite.Percentile(50)

# The peak of the posterior, known as the Maximum Aposteori Probability (MAP)

suite.MAP()

# And a 90% credible interval

suite.CredibleInterval(90)

# We can look up a particular value in the posterior PMF, but the result doesn't mean much, because we could have divided the range (0-100) into as many pieces as we like, and the result would be different.

suite.Prob(50)


# ## Different priors
#
# Let's see how that looks with different priors.
#
# Here's a function that makes a uniform prior:


def UniformPrior(label="uniform"):
    """Makes a Suite with a uniform prior."""
    suite = Euro(range(0, 101), label=label)
    return suite


# And another that makes a triangular prior.


def TrianglePrior(label="triangle"):
    """Makes a Suite with a triangle prior."""
    suite = Euro(label=label)
    for x in range(0, 51):
        suite[x] = x
    for x in range(51, 101):
        suite[x] = 100 - x
    suite.Normalize()
    return suite


# Here's what they look like:

# +
triangle = TrianglePrior()
uniform = UniformPrior()
suites = [triangle, uniform]

thinkplot.Pdfs(suites)
thinkplot.Config(xlabel="x", ylabel="Probability")


# -

# If we update them both with the same data:


def RunUpdate(suite, heads=140, tails=110):
    """Updates the Suite with the given number of heads and tails.

    suite: Suite object
    heads: int
    tails: int
    """
    dataset = "H" * heads + "T" * tails
    for data in dataset:
        suite.Update(data)


for suite in suites:
    RunUpdate(suite)

# The results are almost identical; the remaining difference is unlikely to matter in practice.

thinkplot.Pdfs(suites)
thinkplot.Config(xlabel="x", ylabel="Probability")


# ## The binomial likelihood function
#
# We can make the Euro class more efficient by computing the likelihood of the entire dataset at once, rather than one coin toss at a time.
#
# If the probability of heads is p, we can compute the probability of k=140 heads in n=250 tosses using the binomial PMF.


class Euro2(Suite):
    """Represents hypotheses about the probability of heads."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: integer value of x, the probability of heads (0-100)
        data: tuple of (number of heads, number of tails)
        """
        x = hypo / 100.0
        heads, tails = data
        like = x ** heads * (1 - x) ** tails
        return like


# I left out the binomial coefficient ${n}\choose{k}$ because it does not depend on `p`, so it's the same for all hypotheses.

suite = Euro2(range(0, 101))
dataset = 140, 110
suite.Update(dataset)

# Here's what the posterior looks like.

thinkplot.Pdf(suite)

# ## The Beta distribution
#
# The Beta distribution is a conjugate prior for the binomial likelihood function, which means that if you start with a Beta distribution and update with a binomial likelihood, the posterior is also Beta.
#
# Also, given the parameters of the prior and the data, we can compute the parameters of the posterior directly.  The following class represents a Beta distribution and provides a constant-time Update method.

# +
from scipy import special


class Beta:
    """Represents a Beta distribution.

    See http://en.wikipedia.org/wiki/Beta_distribution
    """

    def __init__(self, alpha=1, beta=1, label=None):
        """Initializes a Beta distribution."""
        self.alpha = alpha
        self.beta = beta
        self.label = label if label is not None else "_nolegend_"

    def Update(self, data):
        """Updates a Beta distribution.

        data: pair of int (heads, tails)
        """
        heads, tails = data
        self.alpha += heads
        self.beta += tails

    def Mean(self):
        """Computes the mean of this distribution."""
        return self.alpha / (self.alpha + self.beta)

    def MAP(self):
        """Computes the value with maximum a posteori probability."""
        a = self.alpha - 1
        b = self.beta - 1
        return a / (a + b)

    def Random(self):
        """Generates a random variate from this distribution."""
        return np.random.betavariate(self.alpha, self.beta)

    def Sample(self, n):
        """Generates a random sample from this distribution.

        n: int sample size
        """
        size = (n,)
        return np.random.beta(self.alpha, self.beta, size)

    def EvalPdf(self, x):
        """Evaluates the PDF at x."""
        return x ** (self.alpha - 1) * (1 - x) ** (self.beta - 1)

    def MakePmf(self, steps=101, label=None):
        """Returns a Pmf of this distribution.

        Note: Normally, we just evaluate the PDF at a sequence
        of points and treat the probability density as a probability
        mass.

        But if alpha or beta is less than one, we have to be
        more careful because the PDF goes to infinity at x=0
        and x=1.  In that case we evaluate the CDF and compute
        differences.

        The result is a little funny, because the values at 0 and 1
        are not symmetric.  Nevertheless, it is a reasonable discrete
        model of the continuous distribution, and behaves well as
        the number of values increases.
        """
        if label is None and self.label is not None:
            label = self.label

        if self.alpha < 1 or self.beta < 1:
            cdf = self.MakeCdf()
            pmf = cdf.MakePmf()
            return pmf

        xs = [i / (steps - 1) for i in range(steps)]
        probs = [self.EvalPdf(x) for x in xs]
        pmf = Pmf(dict(zip(xs, probs)), label=label)
        return pmf

    def MakeCdf(self, steps=101):
        """Returns the CDF of this distribution."""
        xs = [i / (steps - 1) for i in range(steps)]
        ps = special.betainc(self.alpha, self.beta, xs)
        cdf = Cdf(xs, ps)
        return cdf

    def Percentile(self, ps):
        """Returns the given percentiles from this distribution.

        ps: scalar, array, or list of [0-100]
        """
        ps = np.asarray(ps) / 100
        xs = special.betaincinv(self.alpha, self.beta, ps)
        return xs


# -

# Here's how we use it.

beta = Beta()
beta.Update((140, 110))
beta.Mean()

# And here's the posterior.

thinkplot.Pdf(beta.MakePmf())

# Amazing, no?

# **Exercise:** One way to construct priors is to make a Beta distribution and adjust the parameters until it has the shape you want.  Then when you do an update, the data get added to the parameters of the prior.  Since the parameters of the prior play the same mathematical role as the data, they are sometimes called "precounts".
#
# Suppose you believe that most coins are fair or unlikely to deviate from 50% by more than a few percentage points.  Construct a prior that captures this belief and update it with the Euro data.  How much effect does it have on the posterior, compared to the uniform prior?
#
# Hint: A Beta distribution with parameters `(1, 1)` is uniform from 0 to 1.

# +
# Solution

# Here's the uniform prior

uniform = Beta(1, 1, label="uniform")
thinkplot.Pdf(uniform.MakePmf())

# +
# Solution

# And here's what it looks like after the update

uniform.Update(dataset)
thinkplot.Pdf(beta.MakePmf())

# +
# Solution

# Here's a beta prior with precounts chosen to represent
# background knowledge about coins.

beta = Beta(100, 100, label="beta")
thinkplot.Pdf(beta.MakePmf())

# +
# Solution

# And here's what it looks like after the update

beta.Update(dataset)
thinkplot.Pdf(beta.MakePmf())

# +
# Solution

# Comparing the two, we see that the (more) informative
# prior influences the location and spread of the
# posterior.

thinkplot.Pdf(beta.MakePmf())
thinkplot.Pdf(uniform.MakePmf())
thinkplot.Config(xlabel="x", ylabel="Probability")
