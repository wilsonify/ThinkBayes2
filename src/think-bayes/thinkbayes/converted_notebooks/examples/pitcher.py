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
# Copyright 2016 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT

# +
# Configure Jupyter so figures appear in the notebook
# %matplotlib inline

# Configure Jupyter to display the assigned value after an assignment
# %config InteractiveShell.ast_node_interactivity='last_expr_or_assign'

# import classes from thinkbayes2
from thinkbayes2 import Pmf, Suite

import thinkplot


# -

class Subclass(Suite):
    
    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.
        
        data: 
        hypo: 
        """
        like = 1
        return like


prior = Subclass([1,2,3])
thinkplot.Hist(prior)
thinkplot.Config(xlabel='x', ylabel='PMF')

posterior = prior.Copy()
posterior.Update(1)
thinkplot.Hist(prior, color='gray')
thinkplot.Hist(posterior)
thinkplot.Config(xlabel='x', ylabel='PMF')


