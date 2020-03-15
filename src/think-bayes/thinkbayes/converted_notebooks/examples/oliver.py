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

# ## Oliver's blood
#
# Copyright 2018 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT

# +
# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

from thinkbayes2 import Pmf, Suite
# -

# Here is another problem from MacKay’s *Information Theory, Inference, and Learning Algorithms*:
#
# >Two people have left traces of their own blood at the scene of a
# crime. A suspect, Oliver, is tested and found to have type ‘O’
# blood. The blood groups of the two traces are found to be of type
# ‘O’ (a common type in the local population, having frequency 60%)
# and of type ‘AB’ (a rare type, with frequency 1%). Do these data
# (type ‘O’ and ‘AB’ blood were found at scene) give evidence in
# favour of the proposition that Oliver was one of the two people
# present at the crime?
#
# MacKay suggests formulating the problem like this:
#
# >Denote the proposition ‘the suspect and one unknown person were present’ by `S`. The alternative, `S̄`, states ‘two unknown people from the population were present’.
#
# And then he computes the conditional probabilities of the data under `S` and `S̄`.
#
# ```
# P(D | S) = p(AB)
#
# P(D | S̄) = 2 p(O) p(AB)
# ```
#
# Some people are initially unsure why there is a factor of two in the second equation.  One way to convince yourself that it is correct is a verbal argument:  "If Oliver did not leave a blood trace at the scene, then the blood traces were left by two unknown people.  If we consider these unknown people in order, the first might have left type ‘O’ blood and the second might have left type ‘AB’, or the other way around.  Since there are two ways to account for the data, we have to add their probabilities."
#
# This is correct, but with probability it is easy for errors to hide in the words.  I find it useful to express the idea computationally as well.
#
# I'll create a `Pmf` object with the distribution of blood types.

types = Pmf({'O\t': 0.6, 'AB\t':0.01, 'other\t':0.39})
types.Print()

# Now we can compute `P(D | S) = p(AB)`

like_S = types['AB\t']

# `Pmf` provides an addition operator that computes the distribution of all pairs of outcomes:

pairs = types + types
pairs.Print()

# Reading this table, we can see more explicitly that there are two outcomes that account for the data, `AB O` and `O AB`.
#
# So we can compute `P(D | S̄)`:

like_S̄ = pairs['O\tAB\t'] + pairs['AB\tO\t']

# As MacKay points out, the data are more likely under `S̄` than under `S`, so they are evidence in favor of `S̄`; that is, they are exculpatory.
#
# Let's do the update, assuming that the prior is 50:50.

suite = Suite(['S', 'S̄'])
suite.Print()

suite['S'] *= like_S
suite['S̄'] *= like_S̄
suite.Normalize()

# In light of this evidence, we are slightly more inclined to believe that Oliver is not guilty (or at least, did not leave a blood trace at the scene).

suite.Print()
