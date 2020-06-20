"""
Bayesian Zig Zag
Developing probabilistic models using grid methods and MCMC.
Thanks to Chris Fonnesback for his help with this example,
and to Colin Carroll, who added features to pymc3 to support some of these examples.
To install the most current version of pymc3 from source, run

pip3 install -U git+https://github.com/pymc-devs/pymc3.git

Copyright 2018 Allen Downey
MIT License: https://opensource.org/licenses/MIT
"""

import matplotlib.pyplot as plt
import numpy as np
import pymc3 as pm


def test_hockey():
    """
    Simulating hockey
    I'll model hockey as a Poisson process, where each team has some
    long-term average scoring rate, `lambda`, in goals per game.
    For the first example, we'll assume that `lambda` is known (somehow) to be 2.7.
    Since regulation play (as opposed to overtime) is 60 minutes, we can compute the goal scoring rate per minute.
    """

    def half_game(lam_per_min, min_per_game=60):
        """
        If we assume that a goal is equally likely during any minute of the game,
        and we ignore the possibility of scoring more than one goal in the same minute,

        we can simulate a game by generating one random value each minute.
        np.random.random(min_per_game)
        If the random value is less than `lam_per_min`, that means we score a goal during that minute.
        np.random.random(min_per_game) < lam_per_min

        So we can get the number of goals scored by one team like this:
        np.sum(np.random.random(min_per_game) < lam_per_min)

        I'll wrap that in a function.

        :param lam_per_min:
        :param min_per_game:
        :return:
        """
        return np.sum(np.random.random(min_per_game) < lam_per_min)

    # And simulate 10 games.
    lam_per_game = 2.7
    min_per_game = 60
    lam_per_min = lam_per_game / min_per_game
    lam_per_min, lam_per_min ** 2

    size = 10
    sample = [half_game(lam_per_min) for i in range(size)]

    # If we simulate 1000 games, we can see what the distribution looks like.  The average of this sample should be close to `lam_per_game`.

    size = 1000
    sample_sim = [half_game(lam_per_min) for i in range(size)]
    np.mean(sample_sim), lam_per_game

    # ## PMFs
    #
    # To visualize distributions, I'll start with a probability mass function (PMF), which I'll implement using a `Counter`.
    #
    #

    from collections import Counter

    class Pmf(Counter):
        def normalize(self):
            """Normalizes the PMF so the probabilities add to 1."""
            total = sum(self.values())
            for key in self:
                self[key] /= total

        def sorted_items(self):
            """Returns the outcomes and their probabilities."""
            return zip(*sorted(self.items()))

    # Here are some functions for plotting PMFs.

    plot_options = dict(linewidth=3, alpha=0.6)

    def underride(options):
        """Add key-value pairs to d only if key is not in d.

        options: dictionary
        """

        for key, val in plot_options.items():
            options.setdefault(key, val)
        return options

    def plot(xs, ys, **options):
        """Line plot with plot_options."""
        plt.plot(xs, ys, **underride(options))

    def bar(xs, ys, **options):
        """Bar plot with plot_options."""
        plt.bar(xs, ys, **underride(options))

    def plot_pmf(sample, **options):
        """Compute and plot a PMF."""
        pmf = Pmf(sample)
        pmf.normalize()
        xs, ps = pmf.sorted_items()
        bar(xs, ps, **options)

    def pmf_goals():
        """Decorate the axes."""
        plt.xlabel("Number of goals")
        plt.ylabel("PMF")
        plt.title("Distribution of goals scored")
        legend()

    def legend(**options):
        """Draw a legend only if there are labeled items.
        """
        ax = plt.gca()
        handles, labels = ax.get_legend_handles_labels()
        if len(labels):
            plt.legend(**options)

    # Here's what the results from the simulation look like.

    plot_pmf(sample_sim, label="simulation")
    pmf_goals()

    # ## Analytic distributions
    #
    # For the simulation we just did, we can figure out the distribution analytically: it's a binomial distribution with parameters `n` and `p`, where `n` is the number of minutes and `p` is the probability of scoring a goal during any minute.
    #
    # We can use NumPy to generate a sample from a binomial distribution.

    n = min_per_game
    p = lam_per_min
    sample_bin = np.random.binomial(n, p, size)
    np.mean(sample_bin)

    # And confirm that the results are similar to what we got from the model.

    plot_pmf(sample_sim, label="simulation")
    plot_pmf(sample_bin, label="binomial")
    pmf_goals()

    # But plotting PMFs is a bad way to compare distributions.  It's better to use the cumulative distribution function (CDF).

    def plot_cdf(sample, **options):
        """Compute and plot the CDF of a sample."""
        pmf = Pmf(sample)
        xs, freqs = pmf.sorted_items()
        ps = np.cumsum(freqs, dtype=np.float)
        ps /= ps[-1]
        plot(xs, ps, **options)

    def cdf_rates():
        """Decorate the axes."""
        plt.xlabel("Goal scoring rate (mu)")
        plt.ylabel("CDF")
        plt.title("Distribution of goal scoring rate")
        legend()

    def cdf_goals():
        """Decorate the axes."""
        plt.xlabel("Number of goals")
        plt.ylabel("CDF")
        plt.title("Distribution of goals scored")
        legend()

    def plot_cdfs(*sample_seq, **options):
        """Plot multiple CDFs."""
        for sample in sample_seq:
            plot_cdf(sample, **options)
        cdf_goals()

    # Now we can compare the results from the simulation and the sample from the biomial distribution.

    plot_cdf(sample_sim, label="simulation")
    plot_cdf(sample_bin, label="binomial")
    cdf_goals()

    # ## Poisson process
    #
    # For large values of `n`, the binomial distribution converges to the Poisson distribution with parameter `mu = n * p`, which is also `mu = lam_per_game`.

    mu = lam_per_game
    sample_poisson = np.random.poisson(mu, size)
    np.mean(sample_poisson)

    # And we can confirm that the results are consistent with the simulation and the binomial distribution.

    plot_cdfs(sample_sim, sample_bin)
    plot_cdf(sample_poisson, label="poisson", linestyle="dashed")
    legend()

    # ## Warming up PyMC
    #
    # Soon we will want to use `pymc3` to do inference, which is really what it's for.  But just to get warmed up, I will use it to generate a sample from a Poisson distribution.

    model = pm.Model()

    with model:
        goals = pm.Poisson("goals", mu)
        trace = pm.sample_prior_predictive(1000)

    len(trace["goals"])

    sample_pm = trace["goals"]
    np.mean(sample_pm)

    # This example is like using a cannon to kill a fly.  But it help us learn to use the cannon.

    plot_cdfs(sample_sim, sample_bin, sample_poisson)
    plot_cdf(sample_pm, label="poisson pymc", linestyle="dashed")
    legend()

    # ## Evaluating the Poisson distribution
    #
    # One of the nice things about the Poisson distribution is that we can compute its CDF and PMF analytically.  We can use the CDF to check, one more time, the previous results.

    import scipy.stats as st

    xs = np.arange(11)
    ps = st.poisson.cdf(xs, mu)

    plot_cdfs(sample_sim, sample_bin, sample_poisson, sample_pm)
    plt.plot(xs, ps, label="analytic", linestyle="dashed")
    legend()

    # And we can use the PMF to compute the probability of any given outcome.  Here's what the analytic PMF looks like:

    xs = np.arange(11)
    ps = st.poisson.pmf(xs, mu)
    bar(xs, ps, label="analytic PMF")
    pmf_goals()

    # And here's a function that compute the probability of scoring a given number of goals in a game, for a known value of `mu`.

    def poisson_likelihood(goals, mu):
        """Probability of goals given scoring rate.

        goals: observed number of goals (scalar or sequence)
        mu: hypothetical goals per game

        returns: probability
        """
        return np.prod(st.poisson.pmf(goals, mu))

    # Here's the probability of scoring 6 goals in a game if the long-term rate is 2.7 goals per game.

    poisson_likelihood(goals=6, mu=2.7)

    # Here's the probability of scoring 3 goals.

    poisson_likelihood(goals=3, mu=2.7)

    # This function also works with a sequence of goals, so we can compute the probability of scoring 6 goals in the first game and 3 in the second.

    poisson_likelihood(goals=[6, 2], mu=2.7)

    # ## Bayesian inference with grid approximation
    #
    # Ok, it's finally time to do some inference!  The function we just wrote computes the likelihood of the data, given a hypothetical value of `mu`:
    #
    # $\mathrm{Prob}~(x ~|~ \mu)$
    #
    # But what we really want is the distribution of `mu`, given the data:
    #
    # $\mathrm{Prob}~(\mu ~|~ x)$
    #
    # If only there were some theorem that relates these probabilities!
    #
    # The following class implements Bayes's theorem.

    class Suite(Pmf):
        """Represents a set of hypotheses and their probabilities."""

        def bayes_update(self, data, like_func):
            """Perform a Bayesian update.

            data:      some representation of observed data
            like_func: likelihood function that takes (data, hypo), where
                       hypo is the hypothetical value of some parameter,
                       and returns P(data | hypo)
            """
            for hypo in self:
                self[hypo] *= like_func(data, hypo)
            self.normalize()

        def plot(self, **options):
            """Plot the hypotheses and their probabilities."""
            xs, ps = self.sorted_items()
            plot(xs, ps, **options)

    def pdf_rate():
        """Decorate the axes."""
        plt.xlabel("Goals per game (mu)")
        plt.ylabel("PDF")
        plt.title("Distribution of goal scoring rate")
        legend()

    # I'll start with a uniform prior just to keep things simple.  We'll choose a better prior later.

    hypo_mu = np.linspace(0, 20, num=51)
    hypo_mu

    # Initially `suite` represents the prior distribution of `mu`.

    suite = Suite(hypo_mu)
    suite.normalize()
    suite.plot(label="prior")
    pdf_rate()

    # Now we can update it with the data and plot the posterior.

    suite.bayes_update(data=6, like_func=poisson_likelihood)
    suite.plot(label="posterior")
    pdf_rate()

    # With a uniform prior, the posterior is the likelihood function, and the MAP is the value of `mu` that maximizes likelihood, which is the observed number of goals, 6.
    #
    # This result is probably not reasonable, because the prior was not reasonable.

    # ## A better prior
    #
    # To construct a better prior, I'll use scores from previous Stanley Cup finals to estimate the parameters of a gamma distribution.
    #
    # Why gamma?  You'll see.
    #
    # Here are (total goals)/(number of games) for both teams from 2013 to 2017, not including games that went into overtime.

    xs = [13 / 6, 19 / 6, 8 / 4, 4 / 4, 10 / 6, 13 / 6, 2 / 2, 4 / 2, 5 / 3, 6 / 3]

    # If those values were sampled from a gamma distribution, we can estimate its parameters, `k` and `theta`.

    def estimate_gamma_params(xs):
        """Estimate the parameters of a gamma distribution.

        See https://en.wikipedia.org/wiki/Gamma_distribution#Parameter_estimation
        """
        s = np.log(np.mean(xs)) - np.mean(np.log(xs))
        k = (3 - s + np.sqrt((s - 3) ** 2 + 24 * s)) / 12 / s
        theta = np.mean(xs) / k
        alpha = k
        beta = 1 / theta
        return alpha, beta

    # Here are the estimates.

    alpha, beta = estimate_gamma_params(xs)
    print(alpha, beta)

    # The following function takes `alpha` and `beta` and returns a "frozen" distribution from SciPy's stats module:

    def make_gamma_dist(alpha, beta):
        """Returns a frozen distribution with given parameters.
        """
        return st.gamma(a=alpha, scale=1 / beta)

    # The frozen distribution knows how to compute its mean and standard deviation:

    dist = make_gamma_dist(alpha, beta)
    print(dist.mean(), dist.std())

    # And it can compute its PDF.

    hypo_mu = np.linspace(0, 10, num=101)
    ps = dist.pdf(hypo_mu)

    plot(hypo_mu, ps, label="gamma(9.6, 5.1)")
    pdf_rate()

    # We can use `make_gamma_dist` to construct a prior suite with the given parameters.

    def make_gamma_suite(xs, alpha, beta):
        """Makes a suite based on a gamma distribution.

        xs: places to evaluate the PDF
        alpha, beta: parameters of the distribution

        returns: Suite
        """
        dist = make_gamma_dist(alpha, beta)
        ps = dist.pdf(xs)
        prior = Suite(dict(zip(xs, ps)))
        prior.normalize()
        return prior

    # Here's what it looks like.

    prior = make_gamma_suite(hypo_mu, alpha, beta)

    prior.plot(label="gamma prior")
    pdf_rate()

    # And we can update this prior using the observed data.

    posterior = prior.copy()
    posterior.bayes_update(data=6, like_func=poisson_likelihood)

    prior.plot(label="prior")
    posterior.plot(label="posterior")
    pdf_rate()

    # The results are substantially different from what we got with the uniform prior.

    suite.plot(label="posterior with uniform prior", color="gray")
    posterior.plot(label="posterior with gamma prior", color="C1")
    pdf_rate()

    # Suppose the same team plays again and scores 2 goals in the second game.  We can perform a second update using the posterior from the first update as the prior for the second.

    posterior2 = posterior.copy()
    posterior2.bayes_update(data=2, like_func=poisson_likelihood)

    prior.plot(label="prior")
    posterior.plot(label="posterior")
    posterior2.plot(label="posterior2")
    pdf_rate()

    # Or, starting with the original prior, we can update with both pieces of data at the same time.

    posterior3 = prior.copy()
    posterior3.bayes_update(data=[6, 2], like_func=poisson_likelihood)

    prior.plot(label="prior")
    posterior.plot(label="posterior")
    posterior2.plot(label="posterior2")
    posterior3.plot(label="posterior3", linestyle="dashed")
    pdf_rate()

    # ## Update using conjugate priors
    #
    # I'm using a gamma distribution as a prior in part because it has a shape that seems credible based on what I know about hockey.
    #
    # But it is also useful because it happens to be the conjugate prior of the Poisson distribution, which means that if the prior is gamma and we update with a Poisson likelihood function, the posterior is also gamma.
    #
    # See https://en.wikipedia.org/wiki/Conjugate_prior#Discrete_distributions
    #
    # And often we can compute the parameters of the posterior with very little computation.  If we observe `x` goals in `1` game, the new parameters are `alpha+x` and `beta+1`.

    class GammaSuite:
        """Represents a gamma conjugate prior/posterior."""

        def __init__(self, alpha, beta):
            """Initialize.

            alpha, beta: parameters
            dist: frozen distribution from scipy.stats
            """
            self.alpha = alpha
            self.beta = beta
            self.dist = make_gamma_dist(alpha, beta)

        def plot(self, xs, **options):
            """Plot the suite.

            xs: locations where we should evaluate the PDF.
            """
            ps = self.dist.pdf(xs)
            ps /= np.sum(ps)
            plot(xs, ps, **options)

        def bayes_update(self, data):
            return GammaSuite(self.alpha + data, self.beta + 1)

    # Here's what the prior looks like using a `GammaSuite`:

    gamma_prior = GammaSuite(alpha, beta)
    gamma_prior.plot(hypo_mu, label="prior")
    pdf_rate()
    gamma_prior.dist.mean()

    # And here's the posterior after one update.

    gamma_posterior = gamma_prior.bayes_update(6)

    gamma_prior.plot(hypo_mu, label="prior")
    gamma_posterior.plot(hypo_mu, label="posterior")
    pdf_rate()
    gamma_posterior.dist.mean()

    # And we can confirm that the posterior we get using the conjugate prior is the same as the one we got using a grid approximation.

    gamma_prior.plot(hypo_mu, label="prior")
    gamma_posterior.plot(hypo_mu, label="posterior conjugate")
    posterior.plot(label="posterior grid", linestyle="dashed")
    pdf_rate()

    # ## Posterior predictive distribution
    #
    # Ok, let's get to what is usually the point of this whole exercise, making predictions.
    #
    # The prior represents what we believe about the distribution of `mu` based on the data (and our prior beliefs).
    #
    # Each value of `mu` is a possible goal scoring rate.
    #
    # For a given value of `mu`, we can generate a distribution of goals scored in a particular game, which is Poisson.
    #
    # But we don't have a given value of `mu`, we have a whole bunch of values for `mu`, with different probabilities.
    #
    # So the posterior predictive distribution is a mixture of Poissons with different weights.
    #
    # The simplest way to generate the posterior predictive distribution is to
    #
    # 1. Draw a random `mu` from the posterior distribution.
    #
    # 2. Draw a random number of goals from `Poisson(mu)`.
    #
    # 3. Repeat.
    #
    # Here's a function that draws a sample from a posterior `Suite` (the grid approximation, not `GammaSuite`).

    def sample_suite(suite, size):
        """Draw a random sample from a Suite

        suite: Suite object
        size: sample size
        """
        xs, ps = zip(*suite.items())
        return np.random.choice(xs, size, replace=True, p=ps)

    # Here's a sample of `mu` drawn from the posterior distribution (after one game).

    size = 10000
    sample_post = sample_suite(posterior, size)
    np.mean(sample_post)

    # Here's what the posterior distribution looks like.

    plot_cdf(sample_post, label="posterior sample")
    cdf_rates()

    # Now for each value of `mu` in the posterior sample we draw one sample from `Poisson(mu)`

    sample_post_pred = np.random.poisson(sample_post)
    np.mean(sample_post_pred)

    # Here's what the posterior predictive distribution looks like.

    plot_pmf(sample_post_pred, label="posterior predictive sample")
    pmf_goals()

    # ## Posterior prediction done wrong
    #
    # The posterior predictive distribution represents uncertainty from two sources:
    #
    # 1. We don't know `mu`
    #
    # 2. Even if we knew `mu`, we would not know the score of the next game.
    #
    # It is tempting, but wrong, to generate a posterior prediction by taking the mean of the posterior distribution and drawing samples from `Poisson(mu)` with just a single value of `mu`.
    #
    # That's wrong because it eliminates one of our sources of uncertainty.
    #
    # Here's an example:

    mu_mean = np.mean(sample_post)
    sample_post_pred_wrong = np.random.poisson(mu_mean, size)
    np.mean(sample_post_pred_wrong)

    # Here's what the samples looks like:

    plot_cdf(sample_post_pred, label="posterior predictive sample")
    plot_cdf(sample_post_pred_wrong, label="incorrect posterior predictive")
    cdf_goals()

    # In the incorrect predictive sample, low values and high values are slightly less likely.
    #
    # The means are about the same:

    print(np.mean(sample_post_pred), np.mean(sample_post_pred_wrong))

    # But the standard deviation of the incorrect distribution is lower.

    print(np.std(sample_post_pred), np.std(sample_post_pred_wrong))

    # ## Abusing PyMC
    #
    # Ok, we are almost ready to use PyMC for its intended purpose, but first we are going to abuse it a little more.
    #
    # Previously we used PyMC to draw a sample from a Poisson distribution with known `mu`.
    #
    # Now we'll use it to draw a sample from the prior distribution of `mu`, with known `alpha` and `beta`.
    #
    # We still have the values I estimated based on previous playoff finals:

    print(alpha, beta)

    # Now we can draw a sample from the prior predictive distribution:

    model = pm.Model()

    with model:
        mu = pm.Gamma("mu", alpha, beta)
        trace = pm.sample_prior_predictive(1000)

    # This might not be a sensible way to use PyMC.  If we just want to sample from the prior predictive distribution, we could use NumPy or SciPy just as well.  We're doing this to develop and test the model incrementally.
    #
    # So let's see if the sample looks right.

    sample_prior_pm = trace["mu"]
    np.mean(sample_prior_pm)

    sample_prior = sample_suite(prior, 2000)
    np.mean(sample_prior)

    plot_cdf(sample_prior, label="prior")
    plot_cdf(sample_prior_pm, label="prior pymc")
    cdf_rates()

    # It looks pretty good (although not actually as close as I expected).

    # Now let's extend the model to sample from the prior predictive distribution.  This is still a silly way to do it, but it is one more step toward inference.

    model = pm.Model()

    with model:
        mu = pm.Gamma("mu", alpha, beta)
        goals = pm.Poisson("goals", mu, observed=[6])
        trace = pm.sample_prior_predictive(2000)

    # Let's see how the results compare with a sample from the prior predictive distribution, generated by plain old NumPy.

    sample_prior_pred_pm = trace["goals"].flatten()
    np.mean(sample_prior_pred_pm)

    sample_prior_pred = np.random.poisson(sample_prior)
    np.mean(sample_prior_pred)

    # Looks good.

    plot_cdf(sample_prior_pred, label="prior pred")
    plot_cdf(sample_prior_pred_pm, label="prior pred pymc")
    cdf_goals()

    # ## Using PyMC
    #
    # Finally, we are ready to use PyMC for actual inference.  We just have to make one small change.
    #
    # Instead of generating `goals`, we'll mark goals as `observed` and provide the observed data, `6`:

    model = pm.Model()

    with model:
        mu = pm.Gamma("mu", alpha, beta)
        goals = pm.Poisson("goals", mu, observed=6)
        trace = pm.sample(2000, tune=1000)

    # With `goals` fixed, the only unknown is `mu`, so `trace` contains a sample drawn from the posterior distribution of `mu`.  We can plot the posterior using a function provided by PyMC:

    pm.plot_posterior(trace)
    pdf_rate()

    # And we can extract a sample from the posterior of `mu`

    sample_post_pm = trace["mu"]
    np.mean(sample_post_pm)

    # And compare it to the sample we drew from the grid approximation:

    plot_cdf(sample_post, label="posterior grid")
    plot_cdf(sample_post_pm, label="posterior pymc")
    cdf_rates()

    # Again, it looks pretty good.
    #
    # To generate a posterior predictive distribution, we can use `sample_ppc`

    with model:
        post_pred = pm.sample_ppc(trace, samples=2000)

    # Here's what it looks like:

    sample_post_pred_pm = post_pred["goals"]

    sample_post_pred_pm = post_pred["goals"]
    np.mean(sample_post_pred_pm)

    plot_cdf(sample_post_pred, label="posterior pred grid")
    plot_cdf(sample_post_pred_pm, label="posterior pred pm")
    cdf_goals()

    # Look's pretty good!

    # ## Going hierarchical
    #
    # So far, all of this is based on a gamma prior.  To choose the parameters of the prior, I used data from previous Stanley Cup finals and computed a maximum likelihood estimate (MLE).  But that's not correct, because
    #
    # 1. It assumes that the observed goal counts are the long-term goal-scoring rates.
    # 2. It treats `alpha` and `beta` as known values rather than parameters to estimate.
    #
    # In other words, I have ignored two important sources of uncertainty.  As a result, my predictions are almost certainly too confident.
    #
    # The solution is a hierarchical model, where `alpha` and `beta` are the parameters that control `mu` and `mu` is the parameter that controls `goals`.  Then we can use observed `goals` to update the distributions of all three unknown parameters.
    #
    # Of course, now we need a prior distribution for `alpha` and `beta`.  A common choice is the half Cauchy distribution (see [Gelman](http://www.stat.columbia.edu/~gelman/research/published/taumain.pdf)), but on advice of counsel, I'm going with exponential.

    sample = pm.Exponential.dist(lam=1).random(size=1000)
    plot_cdf(sample)
    plt.xscale("log")
    plt.xlabel("Parameter of a gamma distribution")
    plt.ylabel("CDF")
    np.mean(sample)

    # This distribution represents radical uncertainty about the value of this distribution: it's probably between 0.1 and 10, but it could be really big or really small.
    #
    # Here's a PyMC model that generates `alpha` and `beta` from an exponential distribution.

    model = pm.Model()

    with model:
        alpha = pm.Exponential("alpha", lam=1)
        beta = pm.Exponential("beta", lam=1)
        trace = pm.sample_prior_predictive(1000)

    # Here's what the distributions of `alpha` and `beta` look like.

    sample_prior_alpha = trace["alpha"]
    plot_cdf(sample_prior_alpha, label="alpha prior")
    sample_prior_beta = trace["beta"]
    plot_cdf(sample_prior_beta, label="beta prior")

    plt.xscale("log")
    plt.xlabel("Parameter of a gamma distribution")
    plt.ylabel("CDF")
    np.mean(sample_prior_alpha)

    # Now that we have `alpha` and `beta`, we can generate `mu`.

    model = pm.Model()

    with model:
        alpha = pm.Exponential("alpha", lam=1)
        beta = pm.Exponential("beta", lam=1)
        mu = pm.Gamma("mu", alpha, beta)
        trace = pm.sample_prior_predictive(1000)

    # Here's what the prior distribution of `mu` looks like.

    sample_prior_mu = trace["mu"]
    plot_cdf(sample_prior_mu, label="mu prior hierarchical")
    cdf_rates()
    np.mean(sample_prior_mu)

    # In effect, the model is saying "I have never seen a hockey game before.  As far as I know, it could be soccer, could be basketball, could be pinball."
    #
    # If we zoom in on the range 0 to 10, we can compare the prior implied by the hierarchical model with the gamma prior I hand picked.

    plot_cdf(sample_prior_mu, label="mu prior hierarchical")
    plot_cdf(sample_prior, label="mu prior", color="gray")
    plt.xlim(0, 10)
    cdf_rates()

    # Obviously, they are very different.  They agree that the most likely values are less than 10, but the hierarchical model admits the possibility that `mu` could be orders of magnitude bigger.
    #
    # Crazy as it sounds, that's probably what we want in a non-committal prior.
    #
    # Ok, last step of the forward process, let's generate some goals.

    model = pm.Model()

    with model:
        alpha = pm.Exponential("alpha", lam=1)
        beta = pm.Exponential("beta", lam=1)
        mu = pm.Gamma("mu", alpha, beta)
        goals = pm.Poisson("goals", mu)
        trace = pm.sample_prior_predictive(1000)

    # Here's the prior predictive distribution of goals.

    sample_prior_goals = trace["goals"]
    plot_cdf(sample_prior_goals, label="goals prior")
    cdf_goals()
    np.mean(sample_prior_goals)

    # To see whether that distribution is right, I ran samples using SciPy.

    def forward_hierarchical(size=1):
        alpha = st.expon().rvs(size=size)
        beta = st.expon().rvs(size=size)
        mu = st.gamma(a=alpha, scale=1 / beta).rvs(size=size)
        goals = st.poisson(mu).rvs(size=size)
        return goals[0]

    sample_prior_goals_st = [forward_hierarchical() for i in range(1000)]

    plot_cdf(sample_prior_goals, label="goals prior")
    plot_cdf(sample_prior_goals_st, label="goals prior scipy")
    cdf_goals()
    plt.xlim(0, 50)
    plt.legend(loc="lower right")
    np.mean(sample_prior_goals_st)

    # ## Hierarchical inference
    #
    # Once we have the forward process working, we only need a small change to run the reverse process.

    model = pm.Model()

    with model:
        alpha = pm.Exponential("alpha", lam=1)
        beta = pm.Exponential("beta", lam=1)
        mu = pm.Gamma("mu", alpha, beta)
        goals = pm.Poisson("goals", mu, observed=[6])
        trace = pm.sample(1000, tune=2000, nuts_kwargs=dict(target_accept=0.99))

    # Here's the posterior distribution of `mu`.  The posterior mean is close to the observed value, which is what we expect with a weakly informative prior.

    sample_post_mu = trace["mu"]
    plot_cdf(sample_post_mu, label="mu posterior")
    cdf_rates()
    np.mean(sample_post_mu)

    # ## Two teams
    #
    # We can extend the model to estimate different values of `mu` for the two teams.

    model = pm.Model()

    with model:
        alpha = pm.Exponential("alpha", lam=1)
        beta = pm.Exponential("beta", lam=1)
        mu_VGK = pm.Gamma("mu_VGK", alpha, beta)
        mu_WSH = pm.Gamma("mu_WSH", alpha, beta)
        goals_VGK = pm.Poisson("goals_VGK", mu_VGK, observed=[6])
        goals_WSH = pm.Poisson("goals_WSH", mu_WSH, observed=[4])
        trace = pm.sample(1000, tune=2000, nuts_kwargs=dict(target_accept=0.95))

    # We can use `traceplot` to review the results and do some visual diagnostics.

    pm.traceplot(trace)

    # Here are the posterior distribitions for `mu_WSH` and `mu_VGK`.

    sample_post_mu_WSH = trace["mu_WSH"]
    plot_cdf(sample_post_mu_WSH, label="mu_WSH posterior")

    sample_post_mu_VGK = trace["mu_VGK"]
    plot_cdf(sample_post_mu_VGK, label="mu_VGK posterior")

    cdf_rates()
    np.mean(sample_post_mu_WSH), np.mean(sample_post_mu_VGK)

    # On the basis of one game (and never having seen a previous game), here's the probability that Vegas is the better team.

    np.mean(sample_post_mu_VGK > sample_post_mu_WSH)

    # ## More background
    #
    # But let's take advantage of more information.  Here are the results from the five most recent Stanley Cup finals, ignoring games that went into overtime.

    data = dict(
        BOS13=[2, 1, 2],
        CHI13=[0, 3, 3],
        NYR14=[0, 2],
        LAK14=[3, 1],
        TBL15=[1, 4, 3, 1, 1, 0],
        CHI15=[2, 3, 2, 2, 2, 2],
        SJS16=[2, 1, 4, 1],
        PIT16=[3, 3, 2, 3],
        NSH17=[3, 1, 5, 4, 0, 0],
        PIT17=[5, 4, 1, 1, 6, 2],
        VGK18=[6, 2, 1],
        WSH18=[4, 3, 3],
    )

    # Here's how we can get the data into the model.

    model = pm.Model()

    with model:
        alpha = pm.Exponential("alpha", lam=1)
        beta = pm.Exponential("beta", lam=1)

        mu = dict()
        goals = dict()
        for name, observed in data.items():
            mu[name] = pm.Gamma("mu_" + name, alpha, beta)
            goals[name] = pm.Poisson(name, mu[name], observed=observed)

        trace = pm.sample(1000, tune=2000, nuts_kwargs=dict(target_accept=0.95))

    # And here are the results.

    pm.traceplot(trace)

    # Here are the posterior means.

    sample_post_mu_VGK = trace["mu_VGK18"]
    np.mean(sample_post_mu_VGK)

    sample_post_mu_WSH = trace["mu_WSH18"]
    np.mean(sample_post_mu_WSH)

    # They are lower with the background information than without, and closer together.  Here's the updated chance that Vegas is the better team.

    np.mean(sample_post_mu_VGK > sample_post_mu_WSH)

    # ## Predictions
    #
    # Even if Vegas is the better team, that doesn't mean they'll win the next game.
    #
    # We can use `sample_ppc` to generate predictions.

    with model:
        post_pred = pm.sample_ppc(trace, samples=1000)

    # Here are the posterior predictive distributions of goals scored.

    WSH = post_pred["WSH18"]
    WSH.shape

    WSH = post_pred["WSH18"].flatten()
    VGK = post_pred["VGK18"].flatten()

    plot_cdf(WSH, label="WSH")
    plot_cdf(VGK, label="VGK")
    cdf_goals()

    # Here's the chance that Vegas wins the next game.

    win = np.mean(VGK > WSH)
    win

    # The chance that they lose.

    lose = np.mean(WSH > VGK)
    lose

    # And the chance of a tie.

    tie = np.mean(WSH == VGK)
    tie

    # ## Overtime!
    #
    # In the playoffs, you play overtime periods until someone scores.  No stupid shootouts!
    #
    # In a Poisson process with rate parameter `mu`, the time until the next event is exponential with parameter `lam = 1/mu`.
    #
    # So we can take a sample from the posterior distributions of `mu`:

    mu_VGK = trace["mu_VGK18"]
    mu_WSH = trace["mu_WSH18"]

    # And generate time to score,`tts`, for each team:

    tts_VGK = np.random.exponential(1 / mu_VGK)
    np.mean(tts_VGK)

    tts_WSH = np.random.exponential(1 / mu_WSH)
    np.mean(tts_WSH)

    # Here's the chance that Vegas wins in overtime.

    win_ot = np.mean(tts_VGK < tts_WSH)
    win_ot

    # Since `tts` is continuous, ties are unlikely.

    total_win = win + tie * win_ot
    total_win

    # Finally, we can simulate the rest of the series and compute the probability that Vegas wins the series.

    def flip(p):
        """Simulate a single game."""
        return np.random.random() < p

    def series(wins, losses, p_win):
        """Simulate a series.

        wins: number of wins so far
        losses: number of losses so far
        p_win: probability that the team of interest wins a game

        returns: boolean, whether the team of interest wins the series
        """
        while True:
            if flip(p_win):
                wins += 1
            else:
                losses += 1

            if wins == 4:
                return True

            if losses == 4:
                return False

    series(1, 2, total_win)

    t = [series(1, 2, total_win) for i in range(1000)]
    np.mean(t)
