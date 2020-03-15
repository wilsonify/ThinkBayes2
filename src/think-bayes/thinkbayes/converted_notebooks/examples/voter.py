# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.4.0
#   kernelspec:
#     display_name: Python 2
#     language: python
#     name: python2
# ---

# Think Bayes
# -----------
#
# Example problem related to *Think Bayes*
#
# [The MIT License](https://opensource.org/licenses/MIT)
# Copyright 2016 Allen Downey

import thinkbayes

# The following problem was submitted to my blog, [Probably Overthinking It](http://allendowney.blogspot.com/2011/10/my-favorite-bayess-theorem-problems.html), by a user named Amit, who wrote:
#
# > The following data is about a poll that occurred in 3 states. In state1, 50% of voters support Party1, in state2, 60% of the voters support Party1, and in state3, 35% of the voters support Party1. Of the total population of the three states, 40% live in state1, 25% live in state2, and 35% live in state3. Given that a voter supports Party1, what is the probability that he lives in state2?
#
# My solution follows.  First I'll create a suite to represent our prior knowledge.  If we know nothing about a voter, we would use the relative populations of the states to guess where they are from.

prior = thinkbayes.Suite({'State 1': 0.4, 'State 2': 0.25, 'State 3': 0.35})
prior.Print()

# Now if we know a voter supports Party 1, we can use that as data to update our belief.  The following dictionary contains the likelihood of the data (supporting Party 1) under each hypothesis (which state the voter is from).

likelihood = {'State 1': 0.5, 'State 2': 0.60, 'State 3': 0.35}

# To make the posterior distribution, I'll start with a copy of the prior.
#
# The update consists of looping through the hypotheses and multiplying the prior probability of each hypothesis, `hypo`,  by the likelihood of the data if `hypo` is true.
#
# The result is a map from hypotheses to posterior likelihoods, but they are not probabilities yet because they are not normalized.

posterior = prior.Copy()
for hypo in posterior:
    posterior[hypo] *= likelihood[hypo]
posterior.Print()

# Normalizing the posterior distribution returns the total likelihood of the data, which is the normalizing constant.

posterior.Normalize()

# Now the posterior is a proper distribution:

posterior.Print()

# And the probability that the voter is from State 2 is about 32%.


