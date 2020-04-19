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


# # Think Bayes
#
# Copyright 2018 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT


# Configure Jupyter so figures appear in the notebook
# %matplotlib inline

# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

import numpy as np
import pandas as pd

from scipy.stats import poisson

# import classes from thinkbayes
from thinkbayes import Pmf, Cdf, Suite, Joint

import thinkbayes
from thinkbayes import thinkplot

import pymc3 as pm
import theano.tensor as T

# ### Fake data

n = 60
t1 = 30
t2 = n - t1
lam1 = 4
lam2 = 2

before = poisson(lam1).rvs(t1)

after = poisson(lam2).rvs(t2)

data = np.concatenate([before, after])


# ### Grid algorithm


class Change(Suite, Joint):
    def Likelihood(self, data, hypo):
        """
        
        data: array of counts
        hypo: t, lam1, lam2
        """
        # FILL THIS IN
        return 1


# ### MCMC
#
# To implement this model in PyMC, see Chapter 1 of [Bayesian Methods for Hackers](http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Ch1_Introduction_PyMC2.ipynb)
# and this example from [Computational Statistics in Python](http://people.duke.edu/~ccc14/sta-663-2016/16C_PyMC3.html#Changepoint-detection)

# stop

# ### Real data
#
# Some real data, based on [this analysis from the Baltimore Sun](http://www.baltimoresun.com/news/maryland/crime/bs-md-ci-violence-stats-20181018-story.html)


# # !wget https://raw.githubusercontent.com/baltimore-sun-data/2018-shootings-analysis/master/BPD_Part_1_Victim_Based_Crime_Data.csv


df = pd.read_csv("BPD_Part_1_Victim_Based_Crime_Data.csv", parse_dates=[0])
df.head()

df.shape

shootings = df[df.Description.isin(["HOMICIDE", "SHOOTING"]) & (df.Weapon == "FIREARM")]
shootings.shape

grouped = shootings.groupby("CrimeDate")

counts = grouped["Total Incidents"].sum()
counts.head()

index = pd.date_range(counts.index[0], counts.index[-1])

counts = counts.reindex(index, fill_value=0)
counts.head()

counts.plot()
thinkplot.decorate(xlabel="Date", ylabel="Number of shootings")
