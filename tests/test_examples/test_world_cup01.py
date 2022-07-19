"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import logging

import arviz as az
import numpy as np
import pymc3 as pm
import pytest
from scipy.stats import poisson

import thinkbayes
import thinkplot
from thinkbayes import MakeGammaPmf
from thinkbayes import MakePoissonPmf
from thinkbayes import Pmf, Cdf, Suite
from thinkbayes.scripts.hockey import GOALS_PER_GAME_LABEL

POSTERIOR_LABEL = "Posterior after 1 goal"

mean_rate = 1.3
rem_time = 90 - 23


@pytest.fixture(name="mix")
def mix_fix():
    xs = np.linspace(0, 12, 101)
    pmf_gamma = thinkbayes.MakeGammaPmf(xs, 1.3)
    prior = Soccer(pmf_gamma)
    metapmf = Pmf()
    for lam, prob in prior.Items():
        lt = lam * rem_time / 90
        pred = MakePoissonPmf(lt, 15)
        metapmf[pred] = prob
    mix = MakeMixture(metapmf)
    return mix


class Soccer(Suite):
    """
    The World Cup Problem, Part One
    >In the 2014 FIFA World Cup, Germany played Brazil in a semifinal match.
    Germany scored after 11 minutes and again at the 23 minute mark.
    At that point in the match, how many goals would you expect Germany to score after 90 minutes?
    What was the probability that they would score 5 more goals (as, in fact, they did)?

    Let's assume that Germany has some hypothetical goal-scoring rate, λ, in goals per game.
    To represent the prior distribution of λ, I'll use a Gamma distribution with mean 1.3,
    which is the average number of goals per team per game in World Cup play.

    Here's what the prior looks like.

    Represents hypotheses about goal-scoring rates.
    """

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: scoring rate in goals per game
        data: interarrival time in minutes
        """
        x = data / 90
        lam = hypo
        like = lam * np.exp(-lam * x)
        return like


class Soccer2(thinkbayes.Suite):
    """Represents hypotheses about goal-scoring rates."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: goal rate in goals per game
        data: goals scored in a game
        """
        return poisson.pmf(data, hypo)


def MakeMixture(metapmf, label="mix"):
    """
    Make a mixture distribution.

    Args:
      metapmf: Pmf; a Meta-Pmf (a Pmf that contains Pmfs) that maps from Pmfs to probs.
      label: string label for the new Pmf.

    Returns: Pmf object. a single Pmf that represents the weighted mixture of distributions:
    """
    mix = Pmf(label=label)
    for pmf, p1 in metapmf.Items():
        for x, p2 in pmf.Items():
            mix[x] += p1 * p2
    return mix


def PredictiveDist(suite, duration=1, label="pred"):
    """Computes the distribution of goals scored in a game.

    returns: new Pmf (mixture of Poissons)
    """
    metapmf = thinkbayes.Pmf()
    for lam, prob in suite.Items():
        pred = thinkbayes.MakePoissonPmf(lam * duration, 10)
        metapmf[pred] = prob

    mix = thinkbayes.MakeMixture(metapmf, label=label)
    return mix


@pytest.fixture(name="gamma_pmf")
def gamma_pmf_fixture():
    """
    The World Cup Problem, Part One
    In the 2014 FIFA World Cup, Germany played Brazil in a semifinal match.
    Germany scored after 11 minutes and again at the 23 minute mark.
    At that point in the match, how many goals would you expect Germany to score after 90 minutes?
    What was the probability that they would score 5 more goals (as, in fact, they did)?

    Let's assume that Germany has some hypothetical goal-scoring rate, λ, in goals per game.
    To represent the prior distribution of λ, I'll use a Gamma distribution with mean 1.3,
    which is the average number of goals per team per game in World Cup play.
    Here's what the prior looks like.
    """

    xs = np.linspace(0, 8, 101)
    pmf = MakeGammaPmf(xs, 1.3)
    return pmf


def test_soccer(gamma_pmf):
    """
    **Exercise:**
    Write a class called `Soccer` that extends `Suite` and defines `Likelihood`,
    which should compute the probability of the data (the time between goals in minutes)
     for a hypothetical goal-scoring rate, `lam`, in goals per game.

    For a given value of `lam`, the time between goals is distributed exponentially.
    """

    soccer = Soccer(gamma_pmf)
    soccer.Mean()

    # Here's the update after first goal at 11 minutes.

    soccer.Update(11)

    thinkplot.decorate(
        title=POSTERIOR_LABEL, xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF"
    )
    soccer.Mean()

    # Here's the update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).
    #

    soccer.Update(12)
    soccer.Mean()

    # This distribution represents our belief about `lam` after two goals.
    #
    # ## Estimating the predictive distribution
    #
    # Now to predict the number of goals in the remaining 67 minutes.  There are two sources of uncertainty:
    #
    # 1. We don't know the true value of λ.
    #
    # 2. Even if we did we wouldn't know how many goals would be scored.
    #
    # We can quantify both sources of uncertainty at the same time, like this:
    #
    # 1. Choose a random value from the posterior distribution of λ.
    #
    # 2. Use the chosen value to generate a random number of goals.
    #
    # If we run these steps many times, we can estimate the distribution of goals scored.
    #
    # We can sample a value from the posterior like this:

    lam = soccer.Random()
    logging.info("%r", f"lam = {lam}")

    # Given `lam`, the number of goals scored in the remaining 67 minutes
    # comes from the Poisson distribution with parameter `lam * t`, with `t` in units of goals.
    # So we can generate a random value like this:

    t = 67 / 90
    np.random.poisson(lam * t)

    # If we generate a large sample, we can see the shape of the distribution:

    sample = np.random.poisson(lam * t, size=10000)
    pmf = Pmf(sample)

    pmf.Mean()

    # But that's based on a single value of `lam`, so it doesn't take into account both sources of uncertainty.
    # Instead, we should sample values from the posterior distribution and generate one prediction for each.


def test_posterior_distribution(gamma_pmf):
    # **Exercise:** Write a few lines of code to
    #
    # 1. Use `Pmf.Sample` to generate a sample with `n=10000` from the posterior distribution `soccer`.
    #
    # 2. Use `np.random.poisson` to generate a random number of goals from the Poisson distribution
    # with parameter $\lambda t$, where `t` is the remaining time in the game (in units of games).
    #
    # 3. Plot the distribution of the predicted number of goals, and print its mean.
    #
    # 4. What is the probability of scoring 5 or more goals in the remainder of the game?

    # ## Computing the predictive distribution
    #
    # Alternatively, we can compute the predictive distribution by making a mixture of Poisson distributions.
    #
    # `MakePoissonPmf` makes a Pmf that represents a Poisson distribution.
    # If we assume that `lam` is the mean of the posterior,
    # we can generate a predictive distribution for the number of goals in the remainder of the game.
    soccer = Soccer(gamma_pmf)
    lam = soccer.Mean()

    lt = lam * rem_time / 90
    pred = MakePoissonPmf(lt, 10)

    # The predictive mean is about 2 goals.

    pred.Mean()

    # And the chance of scoring 5 more goals is still small.

    pred.ProbGreater(4)

    # But that answer is only approximate because it does not take into account our uncertainty about `lam`.
    #
    # The correct method is to compute a weighted
    # mixture of Poisson distributions, one for each possible value of `lam`.
    #
    # The following figure shows the different predictive distributions for the different values of `lam`.

    for lam, prob in soccer.Items():
        lt = lam * rem_time / 90
        pred = MakePoissonPmf(lt, 14)

    # We can compute the mixture of these distributions by
    # making a Meta-Pmf that maps from each Poisson Pmf to its probability.

    metapmf = Pmf()

    for lam, prob in soccer.Items():
        lt = lam * rem_time / 90
        pred = MakePoissonPmf(lt, 15)
        metapmf[pred] = prob

    # `MakeMixture` takes a Meta-Pmf (a Pmf that contains Pmfs) and
    # returns a single Pmf that represents the weighted mixture of distributions:

    # Here's the result for the World Cup problem.

    mix = MakeMixture(metapmf)
    mix.Print()

    # And here's what the mixture looks like.

    # **Exercise:** Compute the predictive mean and the probability of scoring 5 or more additional goals.

    # Solution goes here


def test_wc2():
    # ### World Cup problem, part two
    #
    # > In the final match of the 2014 FIFA World Cup, Germany defeated Argentina 1-0.  How much evidence does this victory provide that Germany had the better team?  What is the probability that Germany would win a rematch?
    #
    # Scoring in games like soccer and hockey can be modeled by a Poisson process, which assumes that each team, against a given opponent, will score goals at some goal-scoring rate, $\lambda$, and that this rate does not vary; in other words, the probability of scoring a goal is about the same at any point during the game.
    #
    # Based on this modeling decision, we can answer the questions by
    #
    # 1. Defining a prior distribution for each team's goal-scoring rate against the other,
    # 2. Updating the prior based on the outcome of the game,
    # 3. Using the posterior distributions to compute the probability that Germany's goal-scoring rate is higher.
    # 4. Generating a predictive distribution for the number of goals each team would score in a rematch.
    #
    # I'll start with Step 2.

    # ### Step 2: Updating
    #
    # If goal-scoring is a Poisson process, the distribution of goals per game is Poisson with parameter $\lambda$.  To compute the distribution of $\lambda$ we can define a new class that inherits from `thinkbayes.Suite` and provides an appropriate `Likelihood` function:

    # Solution

    # `Likelihood` computes the likelihood of `data` given `hypo`, where `data` is an observed number of goals, and `hypo` is a hypothetical goal-scoring rate in goals per game.  We can compute the likelihood of the data by evaluating the Poisson probability mass function (PMF).
    #
    # Now we can get back to Step 1.
    #
    # ### Step 1: Constructing the prior
    #
    # Before the game starts, what should we believe about each team's goal scoring rate against each other?  We could use previous tournament results to construct the priors, but to keep things simple, I'll just use the average goal-scoring rate from all matches in the tournament, which was 2.67 goals per game (total for both teams).
    #
    # To construct the prior, I use a gamma distribution with a mean of 1.34 goals per game.

    xs = np.linspace(0, 8, 101)
    pmf = MakeGammaPmf(xs, 1.3)
    pmf.Mean()

    suite = Soccer2(pmf)

    germany = suite.Copy(label="Germany")
    argentina = suite.Copy(label="Argentina")
    pmf.Mean()

    # According to this prior, the goal-scoring rates are always greater than zero, with the most likely value (a priori) near 0.5.  Goal scoring rates greater than 5 are considered unlikely.
    #
    # ### Step 3: Comparing posteriors
    #
    # The next step is to compute the posteriors for the two teams:

    germany = suite.Copy(label="Germany")
    argentina = suite.Copy(label="Argentina")
    germany.Update(1)
    argentina.Update(0)

    print("posterior mean Germany", germany.Mean())
    print("posterior mean Argentina", argentina.Mean())

    # `Update` invokes the likelihood function for each hypothetical value of $\lambda$ and updates the distribution accordingly.
    #
    # Since both teams scored fewer goals than the prior mean (1.4), we expect both posterior means to be lower.
    #
    # Here are the posteriors:

    # To answer the first question, "How much evidence does this victory provide that Germany had the better team?", we can compute the posterior probability that Germany had a higher goal-scoring rate:

    post_prob = germany.ProbGreater(argentina)
    print("posterior prob Germany > Argentina", post_prob)

    # Based on the prior distributions, we would have said that Germany had a 50% chance of having the better team, or 1:1 odds.  Based on the posteriors, we would say that Germany has a 70% chance.  We can use the ratio of the prior and posterior odds to compute the Bayes factor, which measures the strength of the evidence.

    prior_odds = 1
    post_odds = post_prob / (1 - post_prob)
    print("posterior odds Germany > Argentina", post_odds)
    k = post_odds / prior_odds
    print("Bayes factor", k)

    # The Bayes factor is about 2.3, which is generally considered weak evidence.
    #
    # Now on to Step 4.

    # ### Step 4: Comparing posterior distributions
    #
    # **Exercise:**  Write a few lines of code to
    #
    # 1. Choose a random value of `lam` from the posterior distribution of each team.
    #
    # 2. Choose a random number of goals for each team, conditioned on the value of `lam` you chose.
    #
    # 3. Run that "simulation" many times and accumulate the distribution of wins, losses, and ties.
    #
    # Use the results to estimate the probability that Germany would win a rematch.

    # Solution

    gdr_goals = poisson.rvs(germany.sample(1000))
    arg_goals = poisson.rvs(argentina.sample(1000))
    np.mean(gdr_goals > arg_goals)

    # Solution

    np.mean(gdr_goals == arg_goals)

    # Solution

    np.mean(gdr_goals < arg_goals)

    # Instead of running simulations, you could compute the posterior predictive distributions explicitly.
    #
    # Write a function called `PredictiveDist` that takes the posterior distribution of $\lambda$ and a duration (in units of games).
    #
    # It should loop through the hypotheses in `suite`, compute the predictive distribution
    # of goals for each hypothesis, and assemble a "meta-Pmf" which is a
    # Pmf that maps from each predictive distribution to its probability.
    #
    # Finally, it should use `MakeMixture` to compute the mixture of the predictive distributions.

    # Solution

    germany_pred = PredictiveDist(germany, label="germany")
    argentina_pred = PredictiveDist(argentina, label="argentina")

    # Using the predictive distributions, we can compute probabilities for the outcomes of a rematch.

    win = germany_pred.prob_greater(argentina_pred)
    lose = germany_pred.prob_less(argentina_pred)
    tie = 1 - (win + lose)

    print("Posterior prob Germany wins rematch", win)
    print("Posterior prob tie", tie)
    print("Posterior prob Argentina wins rematch", lose)


def test_wc():
    """
    create a `Soccer` object and initialize it with the prior Pmf:
    update after the first goal at 11 minutes.
    update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).    
    compute the mixture of these distributions by making a Meta-Pmf that maps from each Poisson Pmf to its probability.
    
    Here's the result for the World Cup problem.
    And here's what the mixture looks like.

    **Exercise:** 
    Compute the predictive mean and the probability of scoring 5 or more additional goals.
    Building the MCMC model incrementally, start with just the prior distribution for `lam`.
    look at the prior predictive distribution for the time between goals (in games).
    for the inverse problem, estimating `lam` based on the first observed gap and with both observed gaps.
    generate a predictive distribution for the time until the next goal (in games).

    **Exercise:** Use PyMC to write a solution to the second World Cup problem:
    In the final match of the 2014 FIFA World Cup, Germany defeated Argentina 1-0.
    How much evidence does this victory provide that Germany had the better team? 
    What is the probability that Germany would win a rematch?
    """
    xs = np.linspace(0, 12, 101)
    pmf_gamma = thinkbayes.MakeGammaPmf(xs, 1.3)
    pmf_gamma.Mean()

    prior = Soccer(pmf_gamma)
    assert prior.Mean() == pytest.approx(1.3, abs=0.1)

    posterior1 = prior.Copy()
    posterior1.Update(11)
    posterior1.Mean()
    assert posterior1.Mean() == pytest.approx(2.0, abs=0.1)

    posterior2 = posterior1.Copy()
    posterior2.Update(12)
    assert posterior2.Mean() == pytest.approx(2.6, abs=0.1)

    metapmf = Pmf()
    for lam, prob in posterior2.Items():
        lt = lam * rem_time / 90
        pred = MakePoissonPmf(lt, 15)
        metapmf[pred] = prob

    mix = MakeMixture(metapmf)
    assert mix.SortedItems() == [
        (0, 0.21517463028972725),
        (1, 0.264302249774796),
        (2, 0.21151498217528292),
        (3, 0.1390906939615146),
        (4, 0.08154152382879802),
        (5, 0.044312142685748304),
        (6, 0.02281453694723827),
        (7, 0.011279294601411206),
        (8, 0.005401795735609347),
        (9, 0.0025207018987942345),
        (10, 0.0011505436156713563),
        (11, 0.0005148715941114398),
        (12, 0.00022615293328951343),
        (13, 9.75179518113058e-05),
        (14, 4.1256397217875616e-05),
        (15, 1.710560897772643e-05)
    ]

    cdf_gamma = pmf_gamma.make_cdf()

    mean_rate = 1.3
    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        trace = pm.sample_prior_predictive(1000)

    lam_sample = trace["lam"]
    assert lam_sample.mean() == pytest.approx(1.28, abs=0.5)

    cdf_lam = Cdf(lam_sample)
    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam)
        trace = pm.sample_prior_predictive(1000)

    gap_sample = trace["gap"]
    assert gap_sample.mean() == pytest.approx(3, abs=2)

    cdf_lam = Cdf(gap_sample)

    first_gap = 11 / 90

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam, observed=first_gap)
        trace = pm.sample(1000, tune=3000)

    lam_sample = trace["lam"]
    assert lam_sample.mean() == pytest.approx(2.0, abs=0.1)
    assert posterior1.mean() == pytest.approx(2.0, abs=0.1)

    cdf_lam = Cdf(lam_sample)
    second_gap = 12 / 90
    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam, observed=[first_gap, second_gap])
        trace = pm.sample(1000, tune=2000)

    lam_sample = trace["lam"]
    assert lam_sample.mean() == pytest.approx(2.65, abs=0.1)
    assert posterior2.mean() == pytest.approx(2.65, abs=0.1)

    cdf_lam = Cdf(lam_sample)

    with model:
        post_pred = pm.sample_prior_predictive(samples=1000)

    gap_sample = post_pred["gap"].flatten()
    assert gap_sample.mean() > 1

    cdf_gap = Cdf(gap_sample)

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        goals = pm.Poisson("goals", lam, observed=1)
        trace = pm.sample(3000, tune=3000)

    lam_sample = trace["lam"]
    assert lam_sample.mean() == pytest.approx(1.15, abs=0.1)

    cdf_lam = Cdf(lam_sample)

    with model:
        post_pred = pm.sample_prior_predictive(samples=3000)

    goal_sample = post_pred["goals"].flatten()
    assert goal_sample.mean() == pytest.approx(1.24, abs=0.1)

    pmf_goals = Pmf(goal_sample)
    assert pmf_goals.Median() == pytest.approx(1, abs=0.1)

    xs = np.linspace(0, 8, 101)
    pmf = MakeGammaPmf(xs, 1.3)
    pmf.mean()

    germany = Soccer2(pmf)
    germany.Update(1)
    germany_pred = PredictiveDist(germany, label="germany")
    assert germany_pred.Mean() == pytest.approx(1.15, abs=0.1)


def test_world_cup():
    """
    The World Cup Problem, Part One
    In the 2014 FIFA World Cup, Germany played Brazil in a semifinal match.
    Germany scored after 11 minutes and again at the 23 minute mark.
    At that point in the match, how many goals would you expect Germany to score after 90 minutes?
    What was the probability that they would score 5 more goals (as, in fact, they did)?

    Let's assume that Germany has some hypothetical goal-scoring rate, λ, in goals per game.
    To represent the prior distribution of λ, I'll use a Gamma distribution with mean 1.3,
    which is the average number of goals per team per game in World Cup play.

    Here's what the prior looks like.
    Now we can create a `Soccer` object and initialize it with the prior Pmf:
    Here's the update after the first goal at 11 minutes.
    Here's the update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).
    We can compute the mixture of these distributions by making a
    Meta-Pmf that maps from each Poisson Pmf to its probability.
    `MakeMixture` takes a Meta-Pmf (a Pmf that contains Pmfs) and returns
    a single Pmf that represents the weighted mixture of distributions:
    Here's the result for the World Cup problem.
    And here's what the mixture looks like.
    """

    xs = np.linspace(0, 12, 101)
    pmf_gamma = MakeGammaPmf(xs, 1.3)
    pmf_gamma.Mean()

    prior = Soccer(pmf_gamma)
    prior.Mean()

    posterior1 = prior.Copy()
    posterior1.Update(11)

    posterior1.Mean()

    posterior2 = posterior1.Copy()
    posterior2.Update(12)

    posterior2.Mean()

    metapmf = Pmf()
    for lam, prob in posterior2.Items():
        lt = lam * rem_time / 90
        pred = MakePoissonPmf(lt, 15)
        metapmf[pred] = prob

    mix = MakeMixture(metapmf)
    mix.Print()


@pytest.mark.skip(reason='pymc3/numpy version conflict')
def test_pymc_wc():
    """
    **Exercise:**
    Use PyMC to write a solution to the second World Cup problem:
    In the final match of the 2014 FIFA World Cup,
    Germany defeated Argentina 1-0.
    How much evidence does this victory provide that Germany had the better team?
    What is the probability that Germany would win a rematch?
    generate a predictive distribution for the time until the next goal (in games).
    """
    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        goals = pm.Poisson("goals", lam, observed=1)
        trace = pm.sample(1000, tune=3000)
    logging.info("%r", f"goals = {goals}")
    logging.info("%r", f"trace = {trace}")
    az.plot_trace(trace)

    lam_sample = trace["lam"]
    assert lam_sample.mean() == pytest.approx(1.14, abs=0.1)
    cdf_lam = Cdf(lam_sample)

    with model:
        post_pred = pm.sample_prior_predictive(samples=1000)

    goal_sample = post_pred["goals"].flatten()
    assert goal_sample.mean() == pytest.approx(1.3, abs=0.1)

    pmf_goals = Pmf(goal_sample)

    xs = np.linspace(0, 8, 101)
    pmf = MakeGammaPmf(xs, 1.3)
    assert pmf.Mean() == pytest.approx(1.3, abs=0.1)

    germany = Soccer2(pmf)
    germany.Update(1)

    germany_pred = PredictiveDist(germany, label="germany")
