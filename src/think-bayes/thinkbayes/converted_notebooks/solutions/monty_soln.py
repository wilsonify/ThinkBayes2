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
from thinkbayes import Hist, Pmf, Suite
# -

# **Exercise:** Let's consider [a more general version of the Monty Hall problem](https://en.wikipedia.org/wiki/Monty_Hall_problem#Other_host_behaviors) where Monty is more unpredictable.  As before, Monty never opens the door you chose (let's call it A) and never opens the door with the prize.  So if you choose the door with the prize, Monty has to decide which door to open.  Suppose he opens B with probability `p` and C with probability `1-p`.
#
# 1.  If you choose A and Monty opens B, what is the probability that the car is behind A, in terms of `p`?
#
# 2.  What if Monty opens C?
#
# Hint: you might want to use SymPy to do the algebra for you. 

from sympy import symbols
p = symbols('p')

# +
# Solution

# Here's the solution if Monty opens B.

pmf = Pmf('ABC')
pmf['A'] *= p
pmf['B'] *= 0
pmf['C'] *= 1
pmf.Normalize()
pmf['A'].simplify()

# +
# Solution

# When p=0.5, the result is what we saw before

pmf['A'].evalf(subs={p:0.5})

# +
# Solution

# When p=0.0, we know for sure that the prize is behind C

pmf['C'].evalf(subs={p:0.0})

# +
# Solution

# And here's the solution if Monty opens C.

pmf = Pmf('ABC')
pmf['A'] *= 1-p
pmf['B'] *= 1
pmf['C'] *= 0
pmf.Normalize()
pmf['A'].simplify()
