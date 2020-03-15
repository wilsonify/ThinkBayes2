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

# import classes from thinkbayes2
from thinkbayes2 import Hist, Pmf, Suite
# -

# ## The cookie problem
#
# Here's the original statement of the cookie problem:
#
# > Suppose there are two bowls of cookies. Bowl 1 contains 30 vanilla cookies and 10 chocolate cookies. Bowl 2 contains 20 of each.
#
# > Now suppose you choose one of the bowls at random and, without looking, select a cookie at random. The cookie is vanilla. What is the probability that it came from Bowl 1?
#
# If we only draw one cookie, this problem is simple, but if we draw more than one cookie, there is a complication: do we replace the cookie after each draw, or not?
#
# If we replace the cookie, the proportion of vanilla and chocolate cookies stays the same, and we can perform multiple updates with the same likelihood function.
#
# If we *don't* replace the cookie, the proportions change and we have to keep track of the number of cookies in each bowl.
#
# **Exercise:**
#
# Modify the solution from the book to handle selection without replacement.
#
# Hint: Add instance variables to the `Cookie` class to represent the hypothetical state of the bowls, and modify the `Likelihood` function accordingly.
#
# To represent the state of a Bowl, you might want to use the `Hist` class from `thinkbayes2`.

# +
# Solution goes here

# +
# Solution goes here

# +
# Solution goes here

# +
# Solution goes here

# +
# Solution goes here

# +
# Solution goes here

# +
# Solution goes here
