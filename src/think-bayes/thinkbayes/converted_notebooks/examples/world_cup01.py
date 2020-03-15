# -*- coding: utf-8 -*-
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

# # Think Bayes
#
# This notebook presents example code and exercise solutions for Think Bayes.
#
# Copyright 2018 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT

# +
# Configure Jupyter so figures appear in the notebook
# %matplotlib inline

# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

# import classes from thinkbayes
from thinkbayes import Pmf, Suite

import thinkbayes
from thinkbayes import thinkplot

import numpy as np
from scipy.special import gamma
# -

# ## The World Cup Problem, Part One
#
# >In the 2014 FIFA World Cup, Germany played Brazil in a semifinal match. Germany scored after 11 minutes and again at the 23 minute mark. At that point in the match, how many goals would you expect Germany to score after 90 minutes? What was the probability that they would score 5 more goals (as, in fact, they did)?

# Let's assume that Germany has some hypothetical goal-scoring rate, λ, in goals per game.
#
# To represent the prior distribution of λ, I'll use a Gamma distribution with mean 1.3, which is the average number of goals per team per game in World Cup play.
#
# Here's what the prior looks like.

# +
from thinkbayes import MakeGammaPmf

xs = np.linspace(0, 8, 101)
pmf = MakeGammaPmf(xs, 1.3)
thinkplot.Pdf(pmf)
thinkplot.decorate(title='Gamma PDF',
                   xlabel='Goals per game',
                   ylabel='PDF')
pmf.Mean()


# -

# **Exercise:**  Write a class called `Soccer` that extends `Suite` and defines `Likelihood`, which should compute the probability of the data (the time between goals in minutes) for a hypothetical goal-scoring rate, `lam`, in goals per game.
#
# Hint: For a given value of `lam`, the time between goals is distributed exponentially.
#
# Here's an outline to get you started:

class Soccer(Suite):
    """Represents hypotheses about goal-scoring rates."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: scoring rate in goals per game
        data: interarrival time in minutes
        """
        return 1


# +
# Solution goes here
# -

# Now we can create a `Soccer` object and initialize it with the prior Pmf:

soccer = Soccer(pmf)
thinkplot.Pdf(soccer)
thinkplot.decorate(title='Gamma prior',
                   xlabel='Goals per game',
                   ylabel='PDF')
soccer.Mean()

# Here's the update after first goal at 11 minutes.

thinkplot.Pdf(soccer, color='0.7')
soccer.Update(11)
thinkplot.Pdf(soccer)
thinkplot.decorate(title='Posterior after 1 goal',
                   xlabel='Goals per game',
                   ylabel='PDF')
soccer.Mean()

# Here's the update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).
#

thinkplot.Pdf(soccer, color='0.7')
soccer.Update(12)
thinkplot.Pdf(soccer)
thinkplot.decorate(title='Posterior after 2 goals',
                   xlabel='Goals per game',
                   ylabel='PDF')
soccer.Mean()

# This distribution represents our belief about `lam` after two goals.
#
# ## Estimating the predictive distribution
#
# Now to predict the number of goals in the remaining 67 minutes.  There are two sources of uncertainty:
#
# 1. We don't know the true value of λ.
#
# 2. Even if we did we wouldn't know how many goals would be scored.
#
# We can quantify both sources of uncertainty at the same time, like this:
#
# 1. Choose a random value from the posterior distribution of λ.
#
# 2. Use the chosen value to generate a random number of goals.
#
# If we run these steps many times, we can estimate the distribution of goals scored.
#
# We can sample a value from the posterior like this:

lam = soccer.Random()
lam

# Given `lam`, the number of goals scored in the remaining 67 minutes comes from the Poisson distribution with parameter `lam * t`, with `t` in units of goals.
#
# So we can generate a random value like this:

t = 67 / 90
np.random.poisson(lam * t)

# If we generate a large sample, we can see the shape of the distribution:

sample = np.random.poisson(lam * t, size=10000)
pmf = Pmf(sample)
thinkplot.Hist(pmf)
thinkplot.decorate(title='Distribution of goals, known lambda',
                   xlabel='Goals scored', 
                   ylabel='PMF')
pmf.Mean()

# But that's based on a single value of `lam`, so it doesn't take into account both sources of uncertainty.  Instead, we should sample values from the posterior distribution and generate one prediction for each.
#
# **Exercise:** Write a few lines of code to
#
# 1. Use `Pmf.Sample` to generate a sample with `n=10000` from the posterior distribution `soccer`.
#
# 2. Use `np.random.poisson` to generate a random number of goals from the Poisson distribution with parameter $\lambda t$, where `t` is the remaining time in the game (in units of games).
#
# 3. Plot the distribution of the predicted number of goals, and print its mean.
#
# 4. What is the probability of scoring 5 or more goals in the remainder of the game?

# +
# Solution goes here

# +
# Solution goes here
# -

# ## Computing the predictive distribution
#
# Alternatively, we can compute the predictive distribution by making a mixture of Poisson distributions.
#
# `MakePoissonPmf` makes a Pmf that represents a Poisson distribution.

from thinkbayes import MakePoissonPmf

# If we assume that `lam` is the mean of the posterior, we can generate a predictive distribution for the number of goals in the remainder of the game.

lam = soccer.Mean()
rem_time = 90 - 23
lt = lam * rem_time / 90
pred = MakePoissonPmf(lt, 10)
thinkplot.Hist(pred)
thinkplot.decorate(title='Distribution of goals, known lambda',
                   xlabel='Goals scored', 
                   ylabel='PMF')

# The predictive mean is about 2 goals.

pred.Mean()

# And the chance of scoring 5 more goals is still small.

pred.ProbGreater(4)

# But that answer is only approximate because it does not take into account our uncertainty about `lam`.
#
# The correct method is to compute a weighted mixture of Poisson distributions, one for each possible value of `lam`.
#
# The following figure shows the different predictive distributions for the different values of `lam`.

# +
for lam, prob in soccer.Items():
    lt = lam * rem_time / 90
    pred = MakePoissonPmf(lt, 14)
    thinkplot.Pdf(pred, color='gray', alpha=0.3, linewidth=0.5)

thinkplot.decorate(title='Distribution of goals, all lambda',
                   xlabel='Goals scored', 
                   ylabel='PMF')
# -

# We can compute the mixture of these distributions by making a Meta-Pmf that maps from each Poisson Pmf to its probability.

# +
metapmf = Pmf()

for lam, prob in soccer.Items():
    lt = lam * rem_time / 90
    pred = MakePoissonPmf(lt, 15)
    metapmf[pred] = prob


# -

# `MakeMixture` takes a Meta-Pmf (a Pmf that contains Pmfs) and returns a single Pmf that represents the weighted mixture of distributions:

def MakeMixture(metapmf, label='mix'):
    """Make a mixture distribution.

    Args:
      metapmf: Pmf that maps from Pmfs to probs.
      label: string label for the new Pmf.

    Returns: Pmf object.
    """
    mix = Pmf(label=label)
    for pmf, p1 in metapmf.Items():
        for x, p2 in pmf.Items():
            mix[x] += p1 * p2
    return mix


# Here's the result for the World Cup problem.

mix = MakeMixture(metapmf)
mix.Print()

# And here's what the mixture looks like.

thinkplot.Hist(mix)
thinkplot.decorate(title='Posterior predictive distribution',
                   xlabel='Goals scored', 
                   ylabel='PMF')

# **Exercise:** Compute the predictive mean and the probability of scoring 5 or more additional goals.

# +
# Solution goes here
# -


