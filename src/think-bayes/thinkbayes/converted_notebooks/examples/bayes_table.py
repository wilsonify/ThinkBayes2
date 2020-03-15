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

# ## Bayesian updates using the table method
#
# This notebook demonstrates a way of doing simple Bayesian updates using the table method, with a Pandas DataFrame as the table.
#
# Copyright 2018 Allen Downey
#
# MIT License: https://opensource.org/licenses/MIT
#

# +
# Configure Jupyter so figures appear in the notebook
# %matplotlib inline

# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

import numpy as np
import pandas as pd


# -

# As an example, I'll use the "cookie problem", which is a version of a classic probability "urn problem".
#
# Suppose there are two bowls of cookies.
#
# * Bowl #1 has 10 chocolate and 30 vanilla.
#
# * Bowl #2 has 20 of each.
#
# You choose a bowl at random, and then pick a cookie at random.  The cookie turns out to be vanilla.  What is the probability that the bowl you picked from is Bowl #1?

# ### The BayesTable class
#
# Here's the class that represents a Bayesian table.

class BayesTable(pd.DataFrame):
    def __init__(self, hypo, prior=1):
        columns = ['hypo', 'prior', 'likelihood', 'unnorm', 'posterior']
        super().__init__(columns=columns)
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


# Here's an instance that represents the two hypotheses: you either chose from Bowl 1 or Bowl 2:

table = BayesTable(['Bowl 1', 'Bowl 2'])

# Since we didn't specify prior probabilities, the default value is equal priors for all hypotheses.
#
# Now we can specify the likelihoods:
#
# * The likelihood of getting a vanilla cookie from Bowl 1 is 3/4.
#
# * The likelihood of getting a vanilla cookie from Bowl 2 is 1/2.
#
# Here's how we plug the likelihoods in:

table.likelihood = [3/4, 1/2]
table

# The next step is to multiply the priors by the likelihoods, which yields the unnormalized posteriors.

table.mult()
table

# Now we can compute the normalized posteriors; `norm` returns the normalization constant.

table.norm()

table

# We can read the posterior probabilities from the last column: the probability that you chose from Bowl 1 is 60%.

# ### Resetting
#
# Suppose you put the first cookie back, stir the bowl, and select another cookie from the same bowl.
#
# If this second cookie is chocolate, what is the probability, now, that you are drawing from Bowl 1?
#
# To solve this problem, we want a new table where the priors in the new table are the posteriors from the old table.  That's what the `reset` method computes:

table2 = table.reset()

# Here are the likelihoods for the second update.

table2.likelihood = [1/4, 1/2]

# We could run `mult` and `norm` again, or run `update`, which does both steps.

table2.update()

# Here are the results.

table2

# But the result is the same.
