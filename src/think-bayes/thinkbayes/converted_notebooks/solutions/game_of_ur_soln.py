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
# This notebook presents code and exercises from Think Bayes, second edition.
#
# Copyright 2018 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT

# +
# Configure Jupyter so figures appear in the notebook
# %matplotlib inline

# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

from thinkbayes import Pmf, Cdf, Suite
from thinkbayes import thinkplot
# -

# ### The Game of Ur problem
#
# In the Royal Game of Ur, players advance tokens along a track with 14 spaces.  To determine how many spaces to advance, a player rolls 4 dice with 4 sides.  Two corners on each die are marked; the other two are not.  The total number of marked corners -- which is 0, 1, 2, 3, or 4 -- is the number of spaces to advance.
#
# For example, if the total on your first roll is 2, you could advance a token to space 2.  If you roll a 3 on the next roll, you could advance the same token to space 5.
#
# Suppose you have a token on space 13.  How many rolls did it take to get there?
#
# Hint: you might want to start by computing the distribution of k given n, where k is the number of the space and n is the number of rolls.
#
# Then think about the prior distribution of n.

# Here's a Pmf that represents one of the 4-sided dice.

die = Pmf([0, 1])

# And here's the outcome of a single roll.

roll = sum([die]*4)


# I'll start with a simulation, which helps in two ways: it makes modeling assumptions explicit and it provides an estimate of the answer.
#
# The following function simulates playing the game over and over; after every roll, it yields the number of rolls and the total so far.  When it gets past the 14th space, it starts over.

def roll_until(iters):
    """Generates observations of the game.
    
    iters: number of observations
    
    yields: number of rolls, total
    """
    for i in range(iters):
        total = 0
        for n in range(1, 1000):
            total += roll.Random()
            if total > 14:
                break
            yield(n, total)


# Now I'll the simulation many times and, every time the token is observed on space 13, record the number of rolls it took to get there.

pmf_sim = Pmf()
for n, k in roll_until(1000000):
    if k == 13:
        pmf_sim[n] += 1

# Here's the distribution of the number of rolls:

pmf_sim.Normalize()

pmf_sim.Print()

thinkplot.Hist(pmf_sim, label='Simulation')
thinkplot.decorate(xlabel='Number of rolls to get to space 13',
                   ylabel='PMF')

# ### Bayes
#
# Now let's think about a Bayesian solution.  It is straight forward to compute the likelihood function, which is the probability of being on space 13 after a hypothetical `n` rolls.
#
# `pmf_n` is the distribution of spaces after `n` rolls.
#
# `pmf_13` is the probability of being on space 13 after `n` rolls.

# +
pmf_13 = Pmf()
for n in range(4, 15):
    pmf_n = sum([roll]*n)
    pmf_13[n] = pmf_n[13]
    
pmf_13.Print()
pmf_13.Total()
# -

# The total probability of the data is very close to 1/2, but it's not obvious (to me) why.
#
# Nevertheless, `pmf_13` is the probability of the data for each hypothetical values of `n`, so it is the likelihood function.
#
# ### The prior
#
# Now we need to think about a prior distribution on the number of rolls.  This is not easy to reason about, so let's start by assuming that it is uniform, and see where that gets us.
#
# If the prior is uniform, the posterior equals the likelihood function, normalized.

posterior = pmf_13.Copy()
posterior.Normalize()
posterior.Print()

# That sure looks similar to what we got by simulation.  Let's compare them.

thinkplot.Hist(pmf_sim, label='Simulation')
thinkplot.Pmf(posterior, color='orange', label='Normalized likelihoods')
thinkplot.decorate(xlabel='Number of rolls (n)',
                   ylabel='PMF')

# Since the posterior distribution based on a uniform prior matches the simulation, it seems like the uniform prior must be correct.  But it is not obvious (to me) why.


