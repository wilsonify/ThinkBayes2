"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np

from thinkbayes import Pmf, Cdf, Suite, Joint

from thinkbayes import thinkplot


def test_ltb():
    # ## Lions and Tigers and Bears
    #
    # Suppose we visit a wild animal preserve where we know that the only animals are lions and tigers and bears, but we don't know how many of each there are.
    #
    # During the tour, we see 3 lions, 2 tigers and one bear.  Assuming that every animal had an equal chance to appear in our sample, estimate the prevalence of each species.
    #
    # What is the probability that the next animal we see is a bear?

    # ### Grid algorithm
    #
    # I'll start with a grid algorithm, enumerating the space of prevalences, `p1`, `p2`, and `p3`, that add up to 1, and computing the likelihood of the data for each triple of prevalences.

    class LionsTigersBears(Suite, Joint):
        def Likelihood(self, data, hypo):
            """

            data: string 'L' , 'T', 'B'
            hypo: p1, p2, p3
            """
            # Fill this in.

    # Solution

    class LionsTigersBears(Suite, Joint):
        def Likelihood(self, data, hypo):
            """

            data: string 'L' , 'T', 'B'
            hypo: p1, p2, p3
            """
            p1, p2, p3 = hypo
            if data == "L":
                return p1
            if data == "T":
                return p2
            if data == "B":
                return p3

    ps = np.linspace(0, 1, 101)

    # Here's a simple way to find eligible triplets, but it is inefficient, and it runs into problems with floating-point approximations.

    def enumerate_triples(ps):
        for p1, p2, p3 in product(ps, ps, ps):
            if p1 + p2 + p3 == 1:
                yield p1, p2, p3

    # As an exercise, write a better version of `enumerate_triples`.

    # Solution

    from itertools import product

    def enumerate_triples(ps):
        for p1, p2 in product(ps, ps):
            if p1 + p2 > 1:
                continue
            p3 = 1 - p1 - p2
            yield p1, p2, p3

    # Now we can initialize the suite.

    suite = LionsTigersBears(enumerate_triples(ps))

    # Here are functions for displaying the distributions

    def plot_marginal_pmfs(joint):
        pmf_lion = joint.Marginal(0)
        pmf_tiger = joint.Marginal(1)
        pmf_bear = joint.Marginal(2)

        thinkplot.plot_pdf_line(pmf_lion, label="lions")
        thinkplot.plot_pdf_line(pmf_tiger, label="tigers")
        thinkplot.plot_pdf_line(pmf_bear, label="bears")

        thinkplot.decorate(xlabel="Prevalence", ylabel="PMF")

    def plot_marginal_cdfs(joint):
        pmf_lion = joint.Marginal(0)
        pmf_tiger = joint.Marginal(1)
        pmf_bear = joint.Marginal(2)

        thinkplot.plot_cdf_line(pmf_lion.make_cdf(), label="lions")
        thinkplot.plot_cdf_line(pmf_tiger.make_cdf(), label="tigers")
        thinkplot.plot_cdf_line(pmf_bear.make_cdf(), label="bears")

        thinkplot.decorate(xlabel="Prevalence", ylabel="CDF")

    # Here are the prior distributions

    plot_marginal_cdfs(suite)

    # Now we can do the update.

    for data in "LLLTTB":
        suite.update(data)

    # And here are the posteriors.

    plot_marginal_cdfs(suite)

    # To get the predictive probability of a bear, we can take the mean of the posterior marginal distribution:

    suite.Marginal(2).Mean()

    # Or we can do a pseudo-update and use the total probability of the data.

    suite.Copy().update("B")

    # ### Using the Dirichlet object

    # The Dirichlet distribution is the conjugate prior for this likelihood function, so we can use the `Dirichlet` object to do the update.
    #
    # The following is a [monkey patch](https://en.wikipedia.org/wiki/Monkey_patch) that gives `Dirichlet` objects a `Marginal` method.

    from thinkbayes import Dirichlet

    def DirichletMarginal(dirichlet, i):
        return dirichlet.marginal_beta(i).MakePmf()

    Dirichlet.Marginal = DirichletMarginal

    # Here are the priors:

    dirichlet = Dirichlet(3)
    plot_marginal_cdfs(dirichlet)

    # Here's the update.

    dirichlet.Update((3, 2, 1))

    # Here are the posterior PDFs.

    plot_marginal_pmfs(dirichlet)

    # And the CDFs.

    plot_marginal_cdfs(dirichlet)

    # And we can confirm that we get the same results as the grid algorithm.

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_marginal_cdfs(suite)

    #

    # ### MCMC
    #
    # Exercise: Implement this model using MCMC.  You might want to start with [this example](http://christianherta.de/lehre/dataScience/bayesian/Multinomial-Dirichlet.slides.php).

    import warnings

    warnings.simplefilter("ignore", FutureWarning)

    import pymc3 as pm

    # Here's the data.

    observed = [0, 0, 0, 1, 1, 2]
    k = len(Pmf(observed))
    a = np.ones(k)

    # Here's the MCMC model:

    # Solution

    model = pm.Model()

    with model:
        ps = pm.Dirichlet("ps", a, shape=a.shape)
        xs = pm.Categorical("xs", ps, observed=observed, shape=1)

    logging.info("%r", f"model = {model}")


    # Solution

    with model:
        start = pm.find_MAP()
        step = pm.Metropolis()
        trace = pm.sample(1000, start=start, step=step, tune=1000)

    # Check the traceplot

    pm.traceplot(trace)

    # And let's see the results.

    def plot_trace_cdfs(trace):
        rows = trace["ps"].transpose()

        cdf_lion = Cdf(rows[0])
        cdf_tiger = Cdf(rows[1])
        cdf_bear = Cdf(rows[2])

        thinkplot.plot_cdf_line(cdf_lion, label="lions")
        thinkplot.plot_cdf_line(cdf_tiger, label="tigers")
        thinkplot.plot_cdf_line(cdf_bear, label="bears")

        thinkplot.decorate(xlabel="Prevalence", ylabel="CDF")

    plot_trace_cdfs(trace)

    # And compare them to what we got with Dirichlet:

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_trace_cdfs(trace)

    # ### Using a Multinomial distribution
    #
    # Here's another solution that uses a Multinomial distribution instead of a Categorical.  In this case, we represent the observed data using just the counts, `[3, 2, 1]`, rather than a specific sequence of observations `[0,0,0,1,1,2]`.
    #
    # I suspect that this is a better option; because it uses a less specific representation of the data (without losing any information), I would expect the probability space to be easier to search.
    #
    # This solution is based on [this excellent notebook](http://nbviewer.jupyter.org/github/WillKoehrsen/probabilistic-programming/blob/master/Allen%20Downey%20Problem.ipynb) from Will Koehrsen.

    animals = ["lions", "tigers", "bears"]
    c = np.array([3, 2, 1])
    a = np.array([1, 1, 1])

    warnings.simplefilter("ignore", UserWarning)

    with pm.Model() as model:
        # Probabilities for each species
        ps = pm.Dirichlet("ps", a=a, shape=3)
        # Observed data is a multinomial distribution with 6 trials
        xs = pm.Multinomial("xs", n=6, p=ps, shape=3, observed=c)

    logging.info("%r", f"model = {model}")


    with model:
        # Sample from the posterior
        trace = pm.sample(draws=1000, tune=1000)

    pm.traceplot(trace)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_trace_cdfs(trace)

    # The results look good.  We can use `summary` to get the posterior means, and other summary stats.

    summary = pm.summary(trace)
    summary.index = animals
    logging.info("%r", f"summary = {summary}")


    # We can also use `plot_posterior` to get a better view of the results.

    ax = pm.plot_posterior(trace, varnames=["ps"])

    for i, a in enumerate(animals):
        ax[i].set_title(a)
