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
from thinkbayes import Pmf, Suite, Beta
from thinkbayes import thinkplot

import numpy as np

# -

# ## The social desirability problem
#
# Whenever you survey people about sensitive issues, you have to deal with [social desirability bias](https://en.wikipedia.org/wiki/Social_desirability_bias), which is the tendency of people to shade their answers in the direction they think shows them in the most positive light.
#
# One of the ways to improve the quality of the results is to collect responses in indirect ways.  For example, [here's a clever way one research group estimated the prevalence of atheists](https://fivethirtyeight.com/features/way-more-americans-may-be-atheists-than-we-thought/).
#
# Another way is [randomized response](https://en.wikipedia.org/wiki/Randomized_response), as described in [this presentation](http://www.soz.unibe.ch/ueber_uns/personen/jann/presentations_by_ben_jann/e131361/e131381/rrt_online07_kassel08_ger.pdf) or [this video](https://www.youtube.com/watch?v=nwJ0qY_rP0A).
#
# As an example, suppose you ask 100 people to flip a coin and:
#
# * If they get heads, they honestly answer the question "Do you believe in God?"
#
# * If they get tails, they flip a second coin and report YES for heads, tails for NO.
#
# Assume that you cannot observe whether they flip one coin or two.
# And suppose you get 55 YESes and 45 NOs.
#
# 1. Estimate the prevalence of believers in the surveyed population (by which, as always, I mean compute a posterior distribution).
#
# 2. How efficient is this method?  That is, how does the width of the posterior distribution compare to the distribution you would get if 100 people answered the question honestly?

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

# +
# Solution goes here
# -
