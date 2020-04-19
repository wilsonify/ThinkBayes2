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
# This notebook presents example code and exercise solutions for Think Bayes.
#
# Copyright 2016 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT


# Configure Jupyter so figures appear in the notebook
# %matplotlib inline

# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

# import classes from thinkbayes
from thinkbayes import Pmf, Beta, MakeBinomialPmf

from thinkbayes import thinkplot

import numpy as np

beta = Beta(5, 5)
prior = beta.MakePmf()
thinkplot.Pdf(prior)
thinkplot.decorate(xlabel="Prob Red Sox win (x)", ylabel="PDF")

# %psource beta.Update


beta.Update((15, 0))
posterior = beta.MakePmf()

thinkplot.Pdf(prior, color="gray", label="prior")
thinkplot.Pdf(posterior, label="posterior")
thinkplot.decorate(xlabel="Prob Red Sox win (x)", ylabel="PDF")

posterior.Mean()

posterior.MAP()

posterior.CredibleInterval()

x = posterior.Random()

np.sum(np.random.random(7) < x)


def simulate(k, dist):
    x = dist.Random()
    return np.sum(np.random.random(k) <= x)


simulate(7, posterior)

sample = [simulate(7, posterior) for i in range(100000)]
thinkplot.Hist(Pmf(sample))

np.mean(np.array(sample) >= 4)
