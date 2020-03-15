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

# **Exercise:** This exercise is from one of my favorite books, David MacKay's "Information Theory, Inference, and Learning Algorithms":
#
# > Elvis Presley had a twin brother who died at birth.  What is the probability that Elvis was an identical twin?"
#     
# To answer this one, you need some background information: According to the Wikipedia article on twins:  "Twins are estimated to be approximately 1.9% of the world population, with monozygotic twins making up 0.2% of the total---and 8% of all twins.''

# +
# Solution

# Here's a Pmf with the prior probability that Elvis 
# was an identical twin (taking the fact that he was a 
# twin as background information)

pmf = Pmf(dict(fraternal=0.92, identical=0.08))

# +
# Solution

# And here's the update.  The data is that the other twin
# was also male, which has likelihood 1 if they were identical
# and only 0.5 if they were fraternal.

pmf['fraternal'] *= 0.5
pmf['identical'] *= 1
pmf.Normalize()
pmf.Print()
