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

import pandas as pd
import numpy as np

# +
from thinkbayes.utils import read_gss

gss = read_gss('../../tests/data/gss_bayes')
gss.head()


# +
def replace_invalid(series, bad_vals, replacement=np.nan):
    series.replace(bad_vals, replacement, inplace=True)
    
replace_invalid(gss.feminist, [0, 8, 9])
replace_invalid(gss.polviews, [0, 8, 9])
replace_invalid(gss.partyid, [8, 9])
replace_invalid(gss.indus10, [0])
replace_invalid(gss.occ10, [0])


# -

def values(series):
    return series.value_counts().sort_index()


# https://gssdataexplorer.norc.org/projects/52787/variables/1698/vshow

values(gss.feminist)

# https://gssdataexplorer.norc.org/projects/52787/variables/178/vshow

values(gss.polviews)

# https://gssdataexplorer.norc.org/projects/52787/variables/141/vshow

values(gss.partyid)

# https://gssdataexplorer.norc.org/projects/52787/variables/82/vshow

values(gss.race)

# https://gssdataexplorer.norc.org/projects/52787/variables/81/vshow

values(gss.sex)

# https://gssdataexplorer.norc.org/projects/52787/variables/17/vshow
#
# 6870	Banking and related activities

values(gss.indus10).head()

np.mean(gss.indus10 == 6870)

(gss.indus10 == 6870).mean()

subset = gss.dropna(subset=['sex', 'polviews', 'partyid', 'indus10'])
subset.shape

#

globals().update(subset)

#

female = sex == 2
values(female)

liberal = polviews <= 2
values(liberal)

democrat = partyid <= 1
values(democrat)

banker = indus10 == 6870
values(banker)

# +
total = 0
for x in banker:
    if x is True:
        total += 1
        
total
# -

total / len(banker)


def prob(A):
    """Probability of A"""
    return A.mean()


def count(A):
    """Number of instances of A"""
    return A.sum()


prob(female)

prob(liberal)

prob(democrat)

prob(banker)

prob(democrat & liberal)

count(banker[female])

prob(banker[female])

prob(female & banker)

prob(banker & female) / prob(female)


def conditional(A, B):
    """Conditional probability of A given B"""
    return prob(A[B])


conditional(banker, female)

conditional(liberal, democrat)

conditional(democrat, liberal)

conditional(democrat, female)


def conjunction(A, B):
    """Probability of both A and B"""
    return prob(A) * conditional(B, A)


prob(liberal & democrat)

conjunction(liberal, democrat)

prob(liberal) * prob(democrat)

conjunction(democrat, liberal)

prob(banker) * conditional(female, banker) / prob(female)


def bayes_theorem(A, B):
    """Conditional probability of A given B, using Bayes's theorem"""
    return prob(A) * conditional(B, A) / prob(B)


bayes_theorem(democrat, liberal)

conditional(banker, female)

conditional(banker, female & liberal)

conditional(banker & democrat, female & liberal)
