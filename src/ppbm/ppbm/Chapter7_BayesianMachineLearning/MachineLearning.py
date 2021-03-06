# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.10.2
#   kernelspec:
#     display_name: Python 2
#     language: python
#     name: python2
# ---

# List of topics to cover:
#
# - Bayesian solution to overfitting
#   - Salisman's solution to the Don't Overfit
# - Predictive distributions; "how do I evaluate testing data?"
# - model fitting, BIC + visualization tools
# - Gaussian Processes
#
#
# Would be nice/cool to cover:
#
# - classification models (using the books text)
# - Bayesian networks?









# +
from IPython.core.display import HTML


def css_styling():
    styles = open("../styles/custom.css", "r").read()
    return HTML(styles)
css_styling()
# -


