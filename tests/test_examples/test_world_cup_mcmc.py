# # Think Bayes
#
# This notebook presents example code and exercise solutions for Think Bayes.
#
# Copyright 2018 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT





from thinkbayes import Pmf, Cdf, Suite

import thinkbayes
from thinkbayes import thinkplot

import numpy as np
from scipy.special import gamma

import pymc3 as pm

# ## The World Cup Problem, Part One
#
# >In the 2014 FIFA World Cup, Germany played Brazil in a semifinal match. Germany scored after 11 minutes and again at the 23 minute mark. At that point in the match, how many goals would you expect Germany to score after 90 minutes? What was the probability that they would score 5 more goals (as, in fact, they did)?

# Let's assume that Germany has some hypothetical goal-scoring rate, λ, in goals per game.
#
# To represent the prior distribution of λ, I'll use a Gamma distribution with mean 1.3, which is the average number of goals per team per game in World Cup play.
#
# Here's what the prior looks like.


from thinkbayes import MakeGammaPmf
def test_wc():

    xs = np.linspace(0, 12, 101)
    pmf_gamma = MakeGammaPmf(xs, 1.3)
    thinkplot.Pdf(pmf_gamma)
    thinkplot.decorate(title="Gamma PDF", xlabel="Goals per game", ylabel="PDF")
    pmf_gamma.Mean()


    class Soccer(Suite):
        """Represents hypotheses about goal-scoring rates."""

        def Likelihood(self, data, hypo):
            """Computes the likelihood of the data under the hypothesis.

            hypo: scoring rate in goals per game
            data: interarrival time in minutes
            """
            x = data / 90
            lam = hypo
            like = lam * np.exp(-lam * x)
            return like


    # Now we can create a `Soccer` object and initialize it with the prior Pmf:

    prior = Soccer(pmf_gamma)
    thinkplot.Pdf(prior)
    thinkplot.decorate(title="Gamma prior", xlabel="Goals per game", ylabel="PDF")
    prior.Mean()

    # Here's the update after the first goal at 11 minutes.


    posterior1 = prior.Copy()
    posterior1.Update(11)

    thinkplot.Pdf(prior, color="0.7")
    thinkplot.Pdf(posterior1)
    thinkplot.decorate(
        title="Posterior after 1 goal", xlabel="Goals per game", ylabel="PDF"
    )
    posterior1.Mean()

    # Here's the update after the second goal at 23 minutes (the time between first and second goals is 12 minutes).
    #


    posterior2 = posterior1.Copy()
    posterior2.Update(12)

    thinkplot.Pdf(prior, color="0.7")
    thinkplot.Pdf(posterior1, color="0.7")
    thinkplot.Pdf(posterior2)

    thinkplot.decorate(
        title="Posterior after 2 goals", xlabel="Goals per game", ylabel="PDF"
    )
    posterior2.Mean()

    from thinkbayes import MakePoissonPmf

    # We can compute the mixture of these distributions by making a Meta-Pmf that maps from each Poisson Pmf to its probability.


    rem_time = 90 - 23

    metapmf = Pmf()
    for lam, prob in posterior2.Items():
        lt = lam * rem_time / 90
        pred = MakePoissonPmf(lt, 15)
        metapmf[pred] = prob


    # `MakeMixture` takes a Meta-Pmf (a Pmf that contains Pmfs) and returns a single Pmf that represents the weighted mixture of distributions:


    def MakeMixture(metapmf, label="mix"):
        """Make a mixture distribution.

        Args:
          metapmf: Pmf that maps from Pmfs to probs.
          label: string label for the new Pmf.

        Returns: Pmf object.
        """
        mix = Pmf(label=label)
        for pmf, p1 in metapmf.Items():
            for x, p2 in pmf.Items():
                mix[x] += p1 * p2
        return mix


    # Here's the result for the World Cup problem.

    mix = MakeMixture(metapmf)
    mix.Print()

    # And here's what the mixture looks like.

    thinkplot.Hist(mix)
    thinkplot.decorate(
        title="Posterior predictive distribution", xlabel="Goals scored", ylabel="PMF"
    )

    # **Exercise:** Compute the predictive mean and the probability of scoring 5 or more additional goals.


    # Solution goes here


    # ## MCMC
    #
    # Building the MCMC model incrementally, start with just the prior distribution for `lam`.

    cdf_gamma = pmf_gamma.MakeCdf()

    mean_rate = 1.3

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        trace = pm.sample_prior_predictive(1000)

    lam_sample = trace["lam"]
    print(lam_sample.mean())

    cdf_lam = Cdf(lam_sample)
    thinkplot.Cdf(cdf_gamma, label="Prior grid")
    thinkplot.Cdf(cdf_lam, label="Prior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # Let's look at the prior predictive distribution for the time between goals (in games).

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        gap = pm.Exponential("gap", lam)
        trace = pm.sample_prior_predictive(1000)

    gap_sample = trace["gap"]
    print(gap_sample.mean())
    cdf_lam = Cdf(gap_sample)

    thinkplot.Cdf(cdf_lam)
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
    print(posterior1.Mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.Cdf(posterior1.MakeCdf(), label="Posterior analytic")
    thinkplot.Cdf(cdf_lam, label="Posterior MCMC")
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
    print(posterior2.Mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.Cdf(posterior2.MakeCdf(), label="Posterior analytic")
    thinkplot.Cdf(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And we can generate a predictive distribution for the time until the next goal (in games).

    with model:
        post_pred = pm.sample_ppc(trace, samples=1000)

    gap_sample = post_pred["gap"].flatten()
    print(gap_sample.mean())

    cdf_gap = Cdf(gap_sample)
    thinkplot.Cdf(cdf_gap)
    thinkplot.decorate(xlabel="Time between goals (games)", ylabel="Cdf")

    # **Exercise:** Use PyMC to write a solution to the second World Cup problem:
    #
    # >In the final match of the 2014 FIFA World Cup, Germany defeated Argentina 1-0. How much evidence does this victory provide that Germany had the better team? What is the probability that Germany would win a rematch?

    with pm.Model() as model:
        lam = pm.Gamma("lam", alpha=mean_rate, beta=1)
        goals = pm.Poisson("goals", lam, observed=1)
        trace = pm.sample(3000, tune=3000)

    pm.traceplot(trace)

    lam_sample = trace["lam"]
    print(lam_sample.mean())
    cdf_lam = Cdf(lam_sample)

    thinkplot.Cdf(cdf_lam, label="Posterior MCMC")
    thinkplot.decorate(xlabel="Goal scoring rate", ylabel="Cdf")

    # And we can generate a predictive distribution for the time until the next goal (in games).

    with model:
        post_pred = pm.sample_ppc(trace, samples=3000)

    goal_sample = post_pred["goals"].flatten()
    print(goal_sample.mean())

    pmf_goals = Pmf(goal_sample)
    thinkplot.Hist(pmf_goals)
    thinkplot.decorate(xlabel="Number of goals", ylabel="Cdf")

    from scipy.stats import poisson


    class Soccer2(thinkbayes.Suite):
        """Represents hypotheses about goal-scoring rates."""

        def Likelihood(self, data, hypo):
            """Computes the likelihood of the data under the hypothesis.

            hypo: goal rate in goals per game
            data: goals scored in a game
            """
            return poisson.pmf(data, hypo)


    from thinkbayes import MakeGammaPmf

    xs = np.linspace(0, 8, 101)
    pmf = MakeGammaPmf(xs, 1.3)
    thinkplot.Pdf(pmf)
    thinkplot.decorate(xlabel="Goal-scoring rate (λ)", ylabel="PMF")
    pmf.Mean()

    germany = Soccer2(pmf)

    germany.Update(1)


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


    germany_pred = PredictiveDist(germany, label="germany")

    thinkplot.Hist(germany_pred, width=0.45, align="right")
    thinkplot.Hist(pmf_goals, width=0.45, align="left")
    thinkplot.decorate(xlabel="Predicted # goals", ylabel="Pmf")

    thinkplot.Cdf(germany_pred.MakeCdf(), label="Grid")
    thinkplot.Cdf(Cdf(goal_sample), label="MCMC")
    thinkplot.decorate(xlabel="Predicted # goals", ylabel="Pmf")
