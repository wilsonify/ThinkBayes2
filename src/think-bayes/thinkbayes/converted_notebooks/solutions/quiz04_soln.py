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

import math
import numpy as np

from thinkbayes2 import Pmf, Suite, Joint
import thinkbayes2
import thinkplot


# -

class Battleship(Suite, Joint):
    lam = 1
    
    def Likelihood(self, hypo, data):
        x_actual, y_actual = hypo
        x_guess, y_guess, result = data
        d = np.hypot(x_guess-x_actual, y_guess-y_actual)
        p_hit = np.exp(-self.lam * d)
        return p if result == 'hit' else 1-p


for t, p in gap.Items():
    arrivals = thinkbayes2.MakePoissonPmf(1.3 * t, 25)
    thinkplot.plot(arrivals, color='C0', linewidth=0.1)
    metapmf[arrivals] = p

gap = thinkbayes2.MakeNormalPmf(7, 1, 3)
thinkplot.plot(gap)

metapmf = thinkbayes2.Pmf()
for t, p in gap.Items():
    arrivals = thinkbayes2.MakePoissonPmf(1.3 * t, 25)
    thinkplot.plot(arrivals, color='C0', linewidth=0.1)
    metapmf[arrivals] = p

mix = thinkbayes2.MakeMixture(metapmf)
mix.Mean()
thinkplot.Hist(mix)
thinkplot.decorate(xlabel='Number of passengers',
                   ylabel='PMF')

mix[10]




