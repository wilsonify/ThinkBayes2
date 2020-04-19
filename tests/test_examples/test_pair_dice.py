


# ## The pair of dice problem
#
# Copyright 2018 Allen Downey
#
# MIT License: https://opensource.org/licenses/MIT
#


# Configure Jupyter so figures appear in the notebook
# %matplotlib inline

# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

import numpy as np
import pandas as pd

from thinkbayes import thinkplot
from thinkbayes import Pmf, Suite

from fractions import Fraction


# ### The BayesTable class
#
# Here's the class that represents a Bayesian table.


class BayesTable(pd.DataFrame):
    def __init__(self, hypo, prior=1, **options):
        columns = ["hypo", "prior", "likelihood", "unnorm", "posterior"]
        super().__init__(columns=columns, **options)
        self.hypo = hypo
        self.prior = prior

    def mult(self):
        self.unnorm = self.prior * self.likelihood

    def norm(self):
        nc = np.sum(self.unnorm)
        self.posterior = self.unnorm / nc
        return nc

    def update(self):
        self.mult()
        return self.norm()

    def reset(self):
        return BayesTable(self.hypo, self.posterior)


# ### The pair of dice problem
#
# Suppose I have a box that contains one each of 4-sided, 6-sided, 8-sided, and 12-sided dice.  I choose two dice at random and roll them without letting you see the die or the outcome.  I report that the sum of the dice is 3.
#
# 1) What is the posterior probability that I rolled each possible pair of the dice?
#
#
# 2) If I roll the same dice again, what is the probability that the sum of the dice is 11?

# **Solution**
#
# I'll start by making a list of possible pairs of dice.


sides = [4, 6, 8, 12]
hypo = []
for die1 in sides:
    for die2 in sides:
        if die2 > die1:
            hypo.append((die1, die2))

hypo

# Here's a `BayesTable` that represents the hypotheses.

table = BayesTable(hypo)

# Since we didn't specify prior probabilities, the default value is equal priors for all hypotheses.  They don't have to be normalized, because we have to normalize the posteriors anyway.
#
# Now we can specify the likelihoods: if the first die has `n1` sides and the second die has `n2` sides, the probability of getting a sum of 3 is
#
# `2 / n1 / n2`
#
# The factor of `2` is there because there are two ways the sum can be 3, either the first die is `1` and the second is `2`, or the other way around.
#
# So the likelihoods are:


for i, row in table.iterrows():
    n1, n2 = row.hypo
    table.loc[i, "likelihood"] = 2 / n1 / n2

table

# Now we can use `update` to compute the posterior probabilities:

table.update()
table

# ### Part two
#
# The second part of the problem asks for the (posterior predictive) probability of getting a total of 11 if we roll the same dice again.
#
# For this, it will be useful to write a more general function that computes the probability of getting a total, `k`, given `n1` and `n2`.
#
# Here's an example with the `4` and `6` sided dice:

n1, n2 = 4, 6
d1 = Pmf(range(1, n1 + 1))
d2 = Pmf(range(1, n2 + 1))
total = d1 + d2
thinkplot.Hist(total)


# And here's the general function:


def prob_total(k, n1, n2):
    d1 = Pmf(range(1, n1 + 1))
    d2 = Pmf(range(1, n2 + 1))
    total = d1 + d2
    return total[k]


# To check the results, I'll compare them to the likelihoods in the previous table:

for i, row in table.iterrows():
    n1, n2 = row.hypo
    p = prob_total(3, n1, n2)
    print(n1, n2, p, p == row.likelihood)

# Now we can answer the second part of the question using the law of total probability.  The chance of getting `11` on the second roll is the
#
# $\sum_{n1, n2} P(n1, n2 ~|~ D) \cdot P(11 ~|~ n1, n2)$
#
# The first term is the posterior probability, which we can read from the table; the second term is `prob_total(11, n1, n2)`.
#
# Here's how we compute the total probability:


total = 0
for i, row in table.iterrows():
    n1, n2 = row.hypo
    p = prob_total(11, n1, n2)
    total += row.posterior * p

total

# This calculation is similar to the first step of the update, so we can also compute it by
#
# 1) Creating a new table with the posteriors from `table`.
#
# 2) Adding the likelihood of getting a total of `11` on the next roll.
#
# 3) Computing the normalizing constant.


table2 = table.reset()
for i, row in table2.iterrows():
    n1, n2 = row.hypo
    table2.loc[i, "likelihood"] = prob_total(11, n1, n2)

table2

table2.update()

table2

# ### Using a Suite

# We can solve this problem more concisely, and more efficiently, using a `Suite`.
#
# First, I'll create `Pmf` object for each die.

dice = {}
for n in sides:
    dice[n] = Pmf(range(1, n + 1))

# And a `Pmf` object for the sum of each pair of dice.

pairs = {}
for n1 in sides:
    for n2 in sides:
        if n2 > n1:
            pairs[n1, n2] = dice[n1] + dice[n2]


# Here's a `Dice` class that implements `Likelihood` by looking up the data, `k`, in the `Pmf` that corresponds to `hypo`:


class Dice(Suite):
    def Likelihood(self, data, hypo):
        """Likelihood of the data given the hypothesis.
        
        data: total of two dice
        hypo: pair of sides
        
        return: probability
        """
        return pairs[hypo][data]


# Here's the prior:

suite = Dice(pairs.keys())
suite.Print()

# And the posterior:

suite.Update(3)
suite.Print()

# And the posterior probability of getting `11` on the next roll.

suite.Update(11)
