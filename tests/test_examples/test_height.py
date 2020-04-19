


# # Think Bayes
#
# This notebook presents code and exercises from Think Bayes, second edition.
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

from thinkbayes import Pmf, Cdf, Suite, Joint
from thinkbayes import thinkplot

# ### The height problem
#
# For adult male residents of the US, the mean and standard deviation of height are 178 cm and 7.7 cm.  For adult female residents the corresponding stats are 163 cm and 7.3 cm.  Suppose you learn that someone is 170 cm tall.  What is the probability that they are male?
#
# Run this analysis again for a range of observed heights from 150 cm to 200 cm, and plot a curve that shows P(male) versus height.  What is the mathematical form of this function?

# To represent the likelihood functions, I'll use `norm` from `scipy.stats`, which returns a "frozen" random variable (RV) that represents a normal distribution with given parameters.
#


from scipy.stats import norm

dist_height = dict(male=norm(178, 7.7), female=norm(163, 7.3))


# Write a class that implements `Likelihood` using the frozen distributions.  Here's starter code:


class Height(Suite):
    def Likelihood(self, data, hypo):
        """
        data: height in cm
        hypo: 'male' or 'female'
        """
        return 1


# Solution goes here


# Here's the prior.

suite = Height(["male", "female"])
for hypo, prob in suite.Items():
    print(hypo, prob)

# And the update:

suite.Update(170)
for hypo, prob in suite.Items():
    print(hypo, prob)

# Compute the probability of being male as a function of height, for a range of values between 150 and 200.


# Solution goes here


# Solution goes here


# If you are curious, you can derive the mathematical form of this curve from the PDF of the normal distribution.

# ### How tall is A?
#
# Suppose I choose two residents of the U.S. at random.  A is taller than B.  How tall is A?
#
# What if I tell you that A is taller than B by more than 5 cm.  How tall is A?
#
# For adult male residents of the US, the mean and standard deviation of height are 178 cm and 7.7 cm.  For adult female residents the corresponding stats are 163 cm and 7.3 cm.

# Here are distributions that represent the heights of men and women in the U.S.

dist_height = dict(male=norm(178, 7.7), female=norm(163, 7.3))

hs = np.linspace(130, 210)
ps = dist_height["male"].pdf(hs)
male_height_pmf = Pmf(dict(zip(hs, ps)))

ps = dist_height["female"].pdf(hs)
female_height_pmf = Pmf(dict(zip(hs, ps)))

thinkplot.Pdf(male_height_pmf, label="Male")
thinkplot.Pdf(female_height_pmf, label="Female")

thinkplot.decorate(
    xlabel="Height (cm)", ylabel="PMF", title="Adult residents of the U.S."
)

# Use `thinkbayes.MakeMixture` to make a `Pmf` that represents the height of all residents of the U.S.


# Solution goes here


# Solution goes here


# Write a class that inherits from Suite and Joint, and provides a Likelihood function that computes the probability of the data under a given hypothesis.


# Solution goes here


# Write a function that initializes your `Suite` with an appropriate prior.


# Solution goes here

mix = Pmf()
suite = thinkplot.make_prior(mix)
suite.Total()

thinkplot.Contour(suite)
thinkplot.decorate(
    xlabel="B Height (cm)", ylabel="A Height (cm)", title="Posterior joint distribution"
)

# Update your `Suite`, then plot the joint distribution and the marginal distribution, and compute the posterior means for `A` and `B`.


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here
