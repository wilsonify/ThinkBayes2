"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import logging

import numpy as np
import pymc3 as pm
import pytest
import thinkbayes
from scipy.stats import poisson
from thinkbayes import Pmf, Cdf, Suite
from thinkbayes import make_gamma_pmf
from thinkbayes import make_poisson_pmf
from thinkbayes import thinkplot
from thinkbayes.scripts.hockey import GOALS_PER_GAME_LABEL

POSTERIOR_LABEL = "Posterior after 1 goal"

mean_rate = 1.3
rem_time = 90 - 23


@pytest.fixture(name="mix")
def mix_fix():
    xs = np.linspace(0, 12, 101)
    pmf_gamma = thinkbayes.make_gamma_pmf(xs, 1.3)
    prior = Soccer(pmf_gamma)
    metapmf = Pmf()
    for lam, prob in prior.items():
        lt = lam * rem_time / 90
        pred = make_poisson_pmf(lt, 15)
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

    def likelihood(self, data, hypo):
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

    def likelihood(self, data, hypo):
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
    for pmf, p1 in metapmf.items():
        for x, p2 in pmf.items():
            mix[x] += p1 * p2
    return mix


def PredictiveDist(suite, duration=1, label="pred"):
    """Computes the distribution of goals scored in a game.

    returns: new Pmf (mixture of Poissons)
    """
    metapmf = thinkbayes.Pmf()
    for lam, prob in suite.items():
        pred = thinkbayes.make_poisson_pmf(lam * duration, 10)
        metapmf[pred] = prob

    mix = thinkbayes.make_mixture(metapmf, label=label)
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
    pmf = make_gamma_pmf(xs, 1.3)
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
    thinkplot.plot_pdf_line(soccer)
    thinkplot.decorate(title="GammaPrior", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF")
    soccer.mean()

    # Here's the update after first goal at 11 minutes.

    thinkplot.plot_pdf_line(soccer, color="0.7")
    soccer.update(11)
    thinkplot.plot_pdf_line(soccer)

    thinkplot.decorate(
        title=POSTERIOR_LABEL, xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF"
    )
    soccer.mean()

    # Here's the update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).
    #

    thinkplot.plot_pdf_line(soccer, color="0.7")
    soccer.update(12)
    thinkplot.plot_pdf_line(soccer)
    thinkplot.decorate(
        title="Posterior after 2 goals", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF"
    )
    soccer.mean()

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

    lam = soccer.random()
    logging.info("%r", f"lam = {lam}")

    # Given `lam`, the number of goals scored in the remaining 67 minutes
    # comes from the Poisson distribution with parameter `lam * t`, with `t` in units of goals.
    # So we can generate a random value like this:

    t = 67 / 90
    np.random.poisson(lam * t)

    # If we generate a large sample, we can see the shape of the distribution:

    sample = np.random.poisson(lam * t, size=10000)
    pmf = Pmf(sample)
    thinkplot.plot_hist_bar(pmf)
    thinkplot.decorate(
        title="Distribution of goals, known lambda", xlabel="Goals scored", ylabel="PMF"
    )
    pmf.mean()
    soccer

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
    lam = soccer.mean()
    rem_time = 90 - 23
    lt = lam * rem_time / 90
    pred = make_poisson_pmf(lt, 10)
    thinkplot.plot_hist_bar(pred)
    thinkplot.decorate(
        title="Distribution of goals, known lambda", xlabel="Goals scored", ylabel="PMF"
    )

    # The predictive mean is about 2 goals.

    pred.mean()

    # And the chance of scoring 5 more goals is still small.

    pred.prob_greater(4)

    # But that answer is only approximate because it does not take into account our uncertainty about `lam`.
    #
    # The correct method is to compute a weighted
    # mixture of Poisson distributions, one for each possible value of `lam`.
    #
    # The following figure shows the different predictive distributions for the different values of `lam`.

    for lam, prob in soccer.items():
        lt = lam * rem_time / 90
        pred = make_poisson_pmf(lt, 14)
        thinkplot.plot_pdf_line(pred, color="gray", alpha=0.3, linewidth=0.5)

    thinkplot.decorate(
        title="Distribution of goals, all lambda", xlabel="Goals scored", ylabel="PMF"
    )

    # We can compute the mixture of these distributions by
    # making a Meta-Pmf that maps from each Poisson Pmf to its probability.

    metapmf = Pmf()

    for lam, prob in soccer.items():
        lt = lam * rem_time / 90
        pred = make_poisson_pmf(lt, 15)
        metapmf[pred] = prob

    # `MakeMixture` takes a Meta-Pmf (a Pmf that contains Pmfs) and
    # returns a single Pmf that represents the weighted mixture of distributions:

    # Here's the result for the World Cup problem.

    mix = MakeMixture(metapmf)
    mix.print()

    # And here's what the mixture looks like.

    thinkplot.plot_hist_bar(mix)
    thinkplot.decorate(
        title="Posterior predictive distribution", xlabel="Goals scored", ylabel="PMF"
    )

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
    pmf = make_gamma_pmf(xs, 1.3)
    thinkplot.plot_pdf_line(pmf)
    thinkplot.decorate(xlabel="Goal-scoring rate (λ)", ylabel="PMF")
    pmf.mean()

    suite = Soccer2(pmf)

    germany = suite.copy(label="Germany")
    argentina = suite.copy(label="Argentina")
    thinkplot.plot_pdf_line(germany)
    thinkplot.plot_pdf_line(argentina)
    thinkplot.decorate(xlabel="Goal-scoring rate (λ)", ylabel="PMF")
    pmf.mean()

    # According to this prior, the goal-scoring rates are always greater than zero, with the most likely value (a priori) near 0.5.  Goal scoring rates greater than 5 are considered unlikely.
    #
    # ### Step 3: Comparing posteriors
    #
    # The next step is to compute the posteriors for the two teams:

    germany = suite.copy(label="Germany")
    argentina = suite.copy(label="Argentina")
    germany.update(1)
    argentina.update(0)

    print("posterior mean Germany", germany.mean())
    print("posterior mean Argentina", argentina.mean())

    # `Update` invokes the likelihood function for each hypothetical value of $\lambda$ and updates the distribution accordingly.
    #
    # Since both teams scored fewer goals than the prior mean (1.4), we expect both posterior means to be lower.
    #
    # Here are the posteriors:

    thinkplot.plot_pdf_line(germany)
    thinkplot.plot_pdf_line(argentina)
    thinkplot.decorate(xlabel="Goal-scoring rate (λ)", ylabel="PMF")

    # To answer the first question, "How much evidence does this victory provide that Germany had the better team?", we can compute the posterior probability that Germany had a higher goal-scoring rate:

    post_prob = germany.prob_greater(argentina)
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

    thinkplot.plot_hist_bar(germany_pred, width=0.45, align="right")
    thinkplot.plot_hist_bar(argentina_pred, width=0.45, align="left")
    thinkplot.decorate(xlabel="Predicted # goals", ylabel="Pmf")

    # Using the predictive distributions, we can compute probabilities for the outcomes of a rematch.

    win = germany_pred.prob_greater(argentina_pred)
    lose = germany_pred.prob_less(argentina_pred)
    tie = 1 - (win + lose)

    print("Posterior prob Germany wins rematch", win)
    print("Posterior prob tie", tie)
    print("Posterior prob Argentina wins rematch", lose)


def test_wc():
    xs = np.linspace(0, 12, 101)
    pmf_gamma = thinkbayes.make_gamma_pmf(xs, 1.3)
    thinkplot.plot_pdf_line(pmf_gamma)
    thinkplot.decorate(title="GammaPDF", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF")
    pmf_gamma.mean()

    # Now we can create a `Soccer` object and initialize it with the prior Pmf:

    prior = Soccer(pmf_gamma)
    thinkplot.plot_pdf_line(prior)
    thinkplot.decorate(title="GammaPrior", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF")
    prior.mean()

    # Here's the update after the first goal at 11 minutes.

    posterior1 = prior.copy()
    posterior1.update(11)

    thinkplot.plot_pdf_line(prior, color="0.7")
    thinkplot.plot_pdf_line(posterior1)
    thinkplot.decorate(
        title=POSTERIOR_LABEL, xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF"
    )
    posterior1.mean()

    # Here's the update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).
    #

    posterior2 = posterior1.copy()
    posterior2.update(12)

    thinkplot.plot_pdf_line(prior, color="0.7")
    thinkplot.plot_pdf_line(posterior1, color="0.7")
    thinkplot.plot_pdf_line(posterior2)

    thinkplot.decorate(
        title="Posterior after 2 goals", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF"
    )
    posterior2.mean()

    # We can compute the mixture of these distributions by making a Meta-Pmf that maps from each Poisson Pmf to its probability.

    rem_time = 90 - 23

    metapmf = Pmf()
    for lam, prob in posterior2.items():
        lt = lam * rem_time / 90
        pred = make_poisson_pmf(lt, 15)
        metapmf[pred] = prob

    # Here's the result for the World Cup problem.

    mix = MakeMixture(metapmf)
    mix.print()

    # And here's what the mixture looks like.

    thinkplot.plot_hist_bar(mix)
    thinkplot.decorate(
        title="Posterior predictive distribution", xlabel="Goals scored", ylabel="PMF"
    )

    # **Exercise:** Compute the predictive mean and the probability of scoring 5 or more additional goals.

    # Solution goes here

    # ## MCMC
    #
    # Building the MCMC model incrementally, start with just the prior distribution for `lam`.

    cdf_gamma = pmf_gamma.make_cdf()

    mean_rate = 1.3

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        trace = pm.sample_prior_predictive(1000)

    lam_sample = trace["lam"]
    print(lam_sample.mean())

    cdf_lam = Cdf(lam_sample)
    thinkplot.plot_cdf_line(cdf_gamma, label="Prior grid")
    thinkplot.plot_cdf_line(cdf_lam, label="Prior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # Let's look at the prior predictive distribution for the time between goals (in games).

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam)
        trace = pm.sample_prior_predictive(1000)

    logging.info("%r", f"gap = {gap}")
    logging.info("%r", f"trace = {trace}")

    gap_sample = trace["gap"]
    print(gap_sample.mean())
    cdf_lam = Cdf(gap_sample)

    thinkplot.plot_cdf_line(cdf_lam)
    thinkplot.decorate(xlabel="Time between goals (games)", ylabel="Cdf")

    # Now we're ready for the inverse problem, estimating `lam` based on the first observed gap.

    first_gap = 11 / 90

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam, observed=first_gap)
        trace = pm.sample(1000, tune=3000)

    pm.traceplot(trace)

    lam_sample = trace["lam"]
    print(lam_sample.mean())
    print(posterior1.mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.plot_cdf_line(posterior1.make_cdf(), label="Posterior analytic")
    thinkplot.plot_cdf_line(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And here's the inverse problem with both observed gaps.

    second_gap = 12 / 90

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam, observed=[first_gap, second_gap])
        trace = pm.sample(1000, tune=2000)

    pm.traceplot(trace)

    lam_sample = trace["lam"]
    print(lam_sample.mean())
    print(posterior2.mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.plot_cdf_line(posterior2.make_cdf(), label="Posterior analytic")
    thinkplot.plot_cdf_line(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And we can generate a predictive distribution for the time until the next goal (in games).

    with model:
        post_pred = pm.sample_ppc(trace, samples=1000)

    gap_sample = post_pred["gap"].flatten()
    print(gap_sample.mean())

    cdf_gap = Cdf(gap_sample)
    thinkplot.plot_cdf_line(cdf_gap)
    thinkplot.decorate(xlabel="Time between goals (games)", ylabel="Cdf")

    # **Exercise:** Use PyMC to write a solution to the second World Cup problem:
    #
    # >In the final match of the 2014 FIFA World Cup, Germany defeated Argentina 1-0. How much evidence does this victory provide that Germany had the better team? What is the probability that Germany would win a rematch?

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        goals = pm.Poisson("goals", lam, observed=1)
        trace = pm.sample(3000, tune=3000)
    logging.info("%r", f"goals = {goals}")
    logging.info("%r", f"trace = {trace}")
    pm.traceplot(trace)

    lam_sample = trace["lam"]
    print(lam_sample.mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.plot_cdf_line(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And we can generate a predictive distribution for the time until the next goal (in games).

    with model:
        post_pred = pm.sample_ppc(trace, samples=3000)

    goal_sample = post_pred["goals"].flatten()
    print(goal_sample.mean())

    pmf_goals = Pmf(goal_sample)
    thinkplot.plot_hist_bar(pmf_goals)
    thinkplot.decorate(xlabel="Number of goals", ylabel="Cdf")

    xs = np.linspace(0, 8, 101)
    pmf = make_gamma_pmf(xs, 1.3)
    thinkplot.plot_pdf_line(pmf)
    thinkplot.decorate(xlabel="Goal-scoring rate (λ)", ylabel="PMF")
    pmf.mean()

    germany = Soccer2(pmf)

    germany.update(1)

    germany_pred = PredictiveDist(germany, label="germany")

    thinkplot.plot_hist_bar(germany_pred, width=0.45, align="right")
    thinkplot.plot_hist_bar(pmf_goals, width=0.45, align="left")
    thinkplot.decorate(xlabel="Predicted # goals", ylabel="Pmf")

    thinkplot.plot_cdf_line(germany_pred.make_cdf(), label="Grid")
    thinkplot.plot_cdf_line(Cdf(goal_sample), label="MCMC")
    thinkplot.decorate(xlabel="Predicted # goals", ylabel="Pmf")


def test_world_cup():
    # ## The World Cup Problem, Part One
    #
    # >In the 2014 FIFA World Cup, Germany played Brazil in a semifinal match. Germany scored after 11 minutes and again at the 23 minute mark. At that point in the match, how many goals would you expect Germany to score after 90 minutes? What was the probability that they would score 5 more goals (as, in fact, they did)?

    # Let's assume that Germany has some hypothetical goal-scoring rate, λ, in goals per game.
    #
    # To represent the prior distribution of λ, I'll use a Gamma distribution with mean 1.3, which is the average number of goals per team per game in World Cup play.
    #
    # Here's what the prior looks like.

    xs = np.linspace(0, 12, 101)
    pmf_gamma = make_gamma_pmf(xs, 1.3)
    thinkplot.plot_pdf_line(pmf_gamma)
    thinkplot.decorate(title="GammaPDF", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF")
    pmf_gamma.mean()

    # Now we can create a `Soccer` object and initialize it with the prior Pmf:

    prior = Soccer(pmf_gamma)
    thinkplot.plot_pdf_line(prior)
    thinkplot.decorate(title="GammaPrior", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF")
    prior.mean()

    # Here's the update after the first goal at 11 minutes.

    posterior1 = prior.copy()
    posterior1.update(11)

    thinkplot.plot_pdf_line(prior, color="0.7")
    thinkplot.plot_pdf_line(posterior1)
    thinkplot.decorate(
        title=POSTERIOR_LABEL, xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF"
    )
    posterior1.mean()

    # Here's the update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).
    #

    posterior2 = posterior1.copy()
    posterior2.update(12)

    thinkplot.plot_pdf_line(prior, color="0.7")
    thinkplot.plot_pdf_line(posterior1, color="0.7")
    thinkplot.plot_pdf_line(posterior2)

    thinkplot.decorate(
        title="Posterior after 2 goals", xlabel=GOALS_PER_GAME_LABEL, ylabel="PDF"
    )
    posterior2.mean()

    # We can compute the mixture of these distributions by making a Meta-Pmf that maps from each Poisson Pmf to its probability.

    rem_time = 90 - 23

    metapmf = Pmf()
    for lam, prob in posterior2.items():
        lt = lam * rem_time / 90
        pred = make_poisson_pmf(lt, 15)
        metapmf[pred] = prob

    # `MakeMixture` takes a Meta-Pmf (a Pmf that contains Pmfs) and returns a single Pmf that represents the weighted mixture of distributions:

    # Here's the result for the World Cup problem.

    mix = MakeMixture(metapmf)
    mix.print()

    # And here's what the mixture looks like.

    thinkplot.plot_hist_bar(mix)
    thinkplot.decorate(
        title="Posterior predictive distribution", xlabel="Goals scored", ylabel="PMF"
    )

    metapmf


def test_score5(mix):
    # **Exercise:** Compute the predictive mean and the probability of scoring 5 or more additional goals.

    # Solution
    logging.info("%r", f"mix.mean() = {mix.mean()}")
    logging.info("%r", f"mix.prob_greater(4) = {mix.prob_greater(4)}")

    # ## MCMC
    #
    # Building the MCMC model incrementally, start with just the prior distribution for `lam`.

    cdf_gamma = pmf_gamma.make_cdf()

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        trace = pm.sample_prior_predictive(1000)

    lam_sample = trace["lam"]
    print(lam_sample.mean())

    cdf_lam = Cdf(lam_sample)
    thinkplot.plot_cdf_line(cdf_gamma, label="Prior grid")
    thinkplot.plot_cdf_line(cdf_lam, label="Prior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # Let's look at the prior predictive distribution for the time between goals (in games).

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam)
        trace = pm.sample_prior_predictive(1000)
    logging.info("%r", f"gap = {gap}")
    logging.info("%r", f"trace = {trace}")
    gap_sample = trace["gap"]
    print(gap_sample.mean())
    cdf_lam = Cdf(gap_sample)

    thinkplot.plot_cdf_line(cdf_lam)
    thinkplot.decorate(xlabel="Time between goals (games)", ylabel="Cdf")

    # Now we're ready for the inverse problem, estimating `lam` based on the first observed gap.

    first_gap = 11 / 90

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam, observed=first_gap)
        trace = pm.sample(1000, tune=3000)

    pm.traceplot(trace)

    lam_sample = trace["lam"]
    print(lam_sample.mean())
    print(posterior1.mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.plot_cdf_line(posterior1.make_cdf(), label="Posterior analytic")
    thinkplot.plot_cdf_line(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And here's the inverse problem with both observed gaps.

    second_gap = 12 / 90

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam, observed=[first_gap, second_gap])
        trace = pm.sample(1000, tune=2000)

    pm.traceplot(trace)

    lam_sample = trace["lam"]
    print(lam_sample.mean())
    print(posterior2.mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.plot_cdf_line(posterior2.make_cdf(), label="Posterior analytic")
    thinkplot.plot_cdf_line(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And we can generate a predictive distribution for the time until the next goal (in games).

    with model:
        post_pred = pm.sample_ppc(trace, samples=1000)

    gap_sample = post_pred["gap"].flatten()
    print(gap_sample.mean())

    cdf_gap = Cdf(gap_sample)
    thinkplot.plot_cdf_line(cdf_gap)
    thinkplot.decorate(xlabel="Time between goals (games)", ylabel="Cdf")


def test_PyMC_wc():
    # **Exercise:** Use PyMC to write a solution to the second World Cup problem:
    #
    # >In the final match of the 2014 FIFA World Cup, Germany defeated Argentina 1-0. How much evidence does this victory provide that Germany had the better team? What is the probability that Germany would win a rematch?

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        goals = pm.Poisson("goals", lam, observed=1)
        trace = pm.sample(1000, tune=3000)
    logging.info("%r", f"goals = {goals}")
    logging.info("%r", f"trace = {trace}")
    pm.traceplot(trace)

    lam_sample = trace["lam"]
    print(lam_sample.mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.plot_cdf_line(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And we can generate a predictive distribution for the time until the next goal (in games).

    with model:
        post_pred = pm.sample_ppc(trace, samples=1000)

    goal_sample = post_pred["goals"].flatten()
    print(goal_sample.mean())

    pmf_goals = Pmf(goal_sample)
    thinkplot.plot_hist_bar(pmf_goals)
    thinkplot.decorate(xlabel="Number of goals", ylabel="Cdf")

    xs = np.linspace(0, 8, 101)
    pmf = make_gamma_pmf(xs, 1.3)
    thinkplot.plot_pdf_line(pmf)
    thinkplot.decorate(xlabel="Goal-scoring rate (λ)", ylabel="PMF")
    pmf.mean()

    germany = Soccer2(pmf)

    germany.update(1)

    germany_pred = PredictiveDist(germany, label="germany")

    thinkplot.plot_hist_bar(germany_pred, width=0.45, align="right")
    thinkplot.plot_hist_bar(pmf_goals, width=0.45, align="left")
    thinkplot.decorate(xlabel="Predicted # goals", ylabel="Pmf")

    thinkplot.plot_cdf_line(germany_pred.make_cdf(), label="Grid")
    thinkplot.plot_cdf_line(Cdf(goal_sample), label="MCMC")
    thinkplot.decorate(xlabel="Predicted # goals", ylabel="Pmf")
