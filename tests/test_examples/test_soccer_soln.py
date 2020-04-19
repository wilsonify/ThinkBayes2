
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


# The World Cup Problem: Germany v. Argentina

#
# Allen Downey
#
# This notebook contains a solution to a problem I posed in my Bayesian statistics class:
#
# > In the 2014 FIFA World Cup, Germany played Brazil in a semifinal
# > match.  Germany scored after 11 minutes and again at the 23 minute
# > mark.  At that point in the match, how many goals would you expect
# > Germany to score after 90 minutes?  What was the probability that they
# > would score 5 more goals?
#
# Scoring in games like soccer and hockey can be (reasonably) well modeled by a Poisson process, which assumes that each team, against a given opponent, will score goals at some goal-scoring rate, $\lambda$, and that this rate is stationary; in other words, the probability of scoring a goal is about the same at any point during the game.
#
# Based on this modeling decision, we can answer the questions by
#
# 1. Defining a prior distribution for Germany's goal-scoring rate against Brazil,
# 2. Updating the prior based on the first two goals, and
# 3. Generating a predictive distribution for the number of goals they would score in the remaining minutes.
#
# My solution uses the ThinkBayes2 framework, which is described in [Think Bayes](http://thinkbayes.com), and summarized in [this notebook](http://nbviewer.ipython.org/github/AllenDowney/ThinkBayes2/blob/master/code/framework.ipynb).
#
# I'll start with Step 2.
#
# ### Step 2: Updating
#
# If goal-scoring is a Poisson process, the distribution of time between goals is exponential with parameter $\lambda$, the goal scoring rate.  In this case we are given as data the inter-arrival time of the first two goals, 11 minutes and 12 minutes.  We can define a new class that inherits from `thinkbayes.Suite` and provides an appropriate `Likelihood` function:


import thinkbayes


class Soccer(thinkbayes.Suite):
    """Represents hypotheses about goal-scoring rates."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: goal rate in goals per game
        data: interarrival time in minutes
        """
        x = data
        lam = hypo / 90
        like = thinkbayes.EvalExponentialPdf(x, lam)
        return like




# `Likelihood` computes the likelihood of `data` given `hypo`, where `data` is an observed time between goals in minutes, and `hypo` is a hypothetical goal-scoring rate in goals per game.
#
# After converting `hypo` to goals per minute, we can compute the likelihood of the data by evaluating the exponential probability density function (PDF).  The result is a density, and therefore not a true probability.  But the result from `Likelihood` only needs to be proportional to the probability of the data; it doesn't have to be a probability.
#
# Now we can get back to Step 1.
#
# ### Step 1: Constructing the prior
#
# Before the game starts, what should we believe about Germany's goal-scoring rate against Brazil?  We could use previous tournament results to construct the prior, but to keep things simple, I'll just use the average goal-scoring rate from all matches in the tournament, which was 2.67 goals per game (total for both teams).
#
# To construct the prior, I'll start with an unrealistic uniform distribution and update it with fake data until the mean matches the observed rate for a single team, 1.34 goals per game.


import numpy
from thinkbayes import thinkplot

hypos = numpy.linspace(0, 12, 201)
suite = Soccer(hypos)
suite.Update(
    134
)  # fake data chosen by trial and error to yield the observed prior mean

thinkplot.Pdf(suite)
suite.Mean()


# Now that we have a prior, we can update with the time of the first goal, 11 minutes.

suite.Update(11)  # time until first goal is 11 minutes
thinkplot.Pdf(suite)
suite.Mean()

# After the first goal, the posterior mean rate is almost 1.9 goals per game.
#
# Now we update with the second goal:

suite.Update(12)  # time between first and second goals is 12 minutes
thinkplot.Pdf(suite)
suite.Mean()


# After the second goal, the posterior mean goal rate is 2.3 goals per game.
#
# Now on to Step 3.
#
# ### Step 3: The predictive distribution
#
# If we knew the actual goal scoring rate, $\lambda$, we could predict how many goals Germany would score in the remaining $t = 90-23$ minutes.  The distribution of goals would be Poisson with parameter $\lambda t$.
#
# We don't actually know $\lambda$, but we can use the posterior distribution of $\lambda$ to generate a predictive distribution for the number of additional goals.


def PredRemaining(suite, rem_time):
    """Plots the predictive distribution for additional number of goals.

    suite: posterior distribution of lam in goals per game
    rem_time: remaining time in the game in minutes
    """
    metapmf = thinkbayes.Pmf()
    for lam, prob in suite.Items():
        lt = lam * rem_time / 90
        pred = thinkbayes.MakePoissonPmf(lt, 15)
        metapmf[pred] = prob
        thinkplot.Pdf(pred, color="gray", alpha=0.3, linewidth=0.5)

    mix = thinkbayes.MakeMixture(metapmf)
    return mix


mix = PredRemaining(suite, 90 - 23)


# `PredRemaining` takes the posterior distribution of $\lambda$ and the remaining game time in minutes (I'm ignoring so-called "injury time").
#
# It loops through the hypotheses in `suite`, computes the predictive distribution of additional goals for each hypothesis, and assembles a "meta-Pmf" which is a Pmf that maps from each predictive distribution to its probability.  The figure shows each of the distributions in the meta-Pmf.
#
# Finally, `PredRemaining` uses `MakeMixture` to compute the mixture of the distributions.  Here's what the predictive distribution looks like.

thinkplot.Hist(mix)
thinkplot.Config(xlim=[-0.5, 10.5])

# After the first two goals, the most likely outcome is that Germany will score once more, but there is a substantial chance of scoring 0 or 2--4 additional goals.
#
# Now we can answer the original questions: what is the chance of scoring 5 or more additional goals:

mix.ProbGreater(4)

# After the first two goals, there was only a 6% chance of scoring 5 more times.  And the expected number of additional goals was only 1.7.

mix.Mean()


# That's the end of this example.  But for completeness (and if you are curious), here is the code for `MakeMixture`:


def MakeMixture(metapmf, label="mix"):
    """Make a mixture distribution.

    Args:
      metapmf: Pmf that maps from Pmfs to probs.
      label: string label for the new Pmf.

    Returns: Pmf object.
    """
    mix = thinkbayes.Pmf(label=label)
    for pmf, p1 in metapmf.Items():
        for x, p2 in pmf.Items():
            mix.Incr(x, p1 * p2)
    return mix
