"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging
from itertools import product

import arviz as az
import numpy as np
import pymc3 as pm
import pytest
from thinkbayes import Dirichlet, thinkplot, Cdf, Pmf
from thinkbayes import Suite, Joint


def dirichlet_marginal(dirichlet, i):
    return dirichlet.marginal_beta(i).make_pmf()


Dirichlet.marginal = dirichlet_marginal


def enumerate_triples(ps):
    for p1, p2 in product(ps, ps):
        if p1 + p2 > 1:
            continue
        p3 = 1 - p1 - p2
        yield p1, p2, p3


class LionsTigersBears(Suite, Joint):
    """
    Lions and Tigers and Bears
    Suppose we visit a wild animal preserve where we know that the only animals are lions and tigers and bears,
    but we don't know how many of each there are.
    During the tour, we see 3 lions, 2 tigers and one bear.
    Assuming that every animal had an equal chance to appear in our sample,
    estimate the prevalence of each species.

    What is the probability that the next animal we see is a bear?
    """

    def likelihood(self, data, hypo):
        """
        
        data: string 'L' , 'T', 'B'
        hypo: p1, p2, p3
        """
        p_lion = hypo[0]
        p_tiger = hypo[1]
        p_bear = hypo[2]
        n_lion = 3 + data.count("L")
        n_tiger = 2 + data.count("T")
        n_bear = 1 + data.count("B")

        return p_lion ** n_lion * p_tiger ** n_tiger * p_bear ** n_bear


class LionsTigersBears2(LionsTigersBears):
    def likelihood(self, data, hypo):
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


def plot_marginal_cdfs(joint):
    pmf_lion = joint.marginal(0)
    pmf_tiger = joint.marginal(1)
    pmf_bear = joint.marginal(2)

    thinkplot.plot_cdf_line(pmf_lion.make_cdf(), label="lions")
    thinkplot.plot_cdf_line(pmf_tiger.make_cdf(), label="tigers")
    thinkplot.plot_cdf_line(pmf_bear.make_cdf(), label="bears")

    thinkplot.decorate(xlabel="Prevalence", ylabel="CDF")


def plot_marginal_pmfs(joint):
    pmf_lion = joint.marginal(0)
    pmf_tiger = joint.marginal(1)
    pmf_bear = joint.marginal(2)

    thinkplot.plot_pdf_line(pmf_lion, label="lions")
    thinkplot.plot_pdf_line(pmf_tiger, label="tigers")
    thinkplot.plot_pdf_line(pmf_bear, label="bears")

    thinkplot.decorate(xlabel="Prevalence", ylabel="PMF")


def plot_trace_cdfs(trace):
    rows = np.asarray(trace['ps']).transpose()
    logging.debug(f"rows={rows}")
    logging.debug(f"rows.shape={rows.shape}")

    cdf_lion = Cdf(rows[0])
    cdf_tiger = Cdf(rows[1])
    cdf_bear = Cdf(rows[2])

    thinkplot.plot_cdf_line(cdf_lion, label="lions")
    thinkplot.plot_cdf_line(cdf_tiger, label="tigers")
    thinkplot.plot_cdf_line(cdf_bear, label="bears")

    thinkplot.decorate(xlabel="Prevalence", ylabel="CDF")


@pytest.fixture(name="suite")
def suite_fixture():
    ps = np.linspace(0, 1, 101)
    ps = np.round(ps, 2)
    suite = LionsTigersBears(enumerate_triples(ps))
    return suite


def test_ltb(suite):
    plot_marginal_pmfs(suite)

    for data in "LLLTTB":
        print(data)
        suite.update([data])

    plot_marginal_pmfs(suite)

    plot_marginal_cdfs(suite)


def test_dirichlet(suite):
    dirichlet = Dirichlet(3)
    plot_marginal_pmfs(dirichlet)
    dirichlet.update((3, 2, 1))

    plot_marginal_pmfs(dirichlet)

    plot_marginal_cdfs(dirichlet)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_marginal_cdfs(suite)


def test_mcmc(suite):
    """
    MCMC
    Implement this model using MCMC.
    You might want to start with
    [this example](http://christianherta.de/lehre/dataScience/bayesian/Multinomial-Dirichlet.slides.php).
    """

    dirichlet = Dirichlet(3)

    observed = [0, 0, 0, 1, 1, 2]
    k = len(Pmf(observed))
    a = np.ones(k)

    def create_model(data):
        with pm.Model() as model:
            k = len(Pmf(observed))
            a = np.ones(k)
            p = pm.Dirichlet("p", a, shape=a.shape)
            c = pm.Categorical("c", p, observed=data, shape=1)
        return model

    model = create_model(observed)

    with model:
        step = pm.Metropolis(model.vars)
        trace = dict(ps=pm.sample(100, step))
        # a = pm.traceplot(trace)

    # plot_trace_cdfs(trace)
    # pmf = Pmf(trace['xs'][0])
    # thinkplot.Hist(pmf)

    with model:
        start = pm.find_MAP()
        step = pm.Metropolis()
        trace = pm.sample(1000, start=start, step=step, tune=100)

    # pm.traceplot(trace)
    # plot_trace_cdfs(trace)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    # plot_trace_cdfs(trace)


def test_ltb3():
    """
    Lions and Tigers and Bears
    Suppose we visit a wild animal preserve where we know that the only animals are lions and tigers and bears,
    but we don't know how many of each there are.

    During the tour, we see 3 lions, 2 tigers and one bear.
    Assuming that every animal had an equal chance to appear in our sample, estimate the prevalence of each species.

    What is the probability that the next animal we see is a bear?

    Grid algorithm
    I'll start with a grid algorithm, enumerating the space of prevalences,
    `p1`, `p2`, and `p3`, that add up to 1, and
    computing the likelihood of the data for each triple of prevalences.

    The Dirichlet distribution is the conjugate prior for this likelihood function,
    so we can use the `Dirichlet` object to do the update.
    """

    ps = np.linspace(0, 1, 101)
    suite = LionsTigersBears(enumerate_triples(ps))

    plot_marginal_cdfs(suite)

    for data in "LLLTTB":
        suite.update(data)

    plot_marginal_cdfs(suite)

    probability_bear = suite.marginal(2).mean()
    logging.info(f"probability_bear = {probability_bear}")

    probability_bear_pseudo_update = suite.copy().update("B")
    logging.info(f"probability_bear_pseudo_update = {probability_bear_pseudo_update}")

    dirichlet = Dirichlet(3)
    plot_marginal_cdfs(dirichlet)
    dirichlet.update((3, 2, 1))
    plot_marginal_pmfs(dirichlet)
    plot_marginal_cdfs(dirichlet)
    thinkplot.pre_plot(6)  # same results as the grid algorithm.
    plot_marginal_cdfs(dirichlet)
    plot_marginal_cdfs(suite)

    observed = [0, 0, 0, 1, 1, 2]
    k = len(Pmf(observed))
    a = np.ones(k)

    with pm.Model():
        ps = pm.Dirichlet("ps", a, shape=a.shape)
        xs = pm.Categorical("xs", ps, observed=observed, shape=1)
        start = pm.find_MAP()
        step = pm.Metropolis()
        trace = pm.sample(1000, start=start, step=step, tune=100)

    az.plot_trace(trace)

    logging.debug(f"trace={trace}")
    plot_trace_cdfs(trace)

    # And compare them to what we got with Dirichlet:

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_trace_cdfs(trace)

    animals = ["lions", "tigers", "bears"]
    c = np.array([3, 2, 1])
    a = np.array([1, 1, 1])

    with pm.Model() as model:
        ps = pm.Dirichlet("ps", a=a, shape=3)  # Probabilities for each species
        xs = pm.Multinomial(
            "xs",
            n=6,
            p=ps,
            shape=3,
            observed=c
        )  # Observed data is a multinomial distribution with 6 trials

    logging.info("%r", f"model = {model}")

    with model:
        trace = pm.sample(draws=100, tune=100)  # Sample from the posterior

    pm.traceplot(trace)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_trace_cdfs(trace)

    summary = pm.summary(trace)  # `summary` to get the posterior means, and other summary stats.
    summary.index = animals
    logging.info("%r", f"summary = {summary}")

    ax = pm.plot_posterior(trace, varnames=["ps"])  # `plot_posterior` to get a better view of the results.

    for i, a in enumerate(animals):
        ax[i].set_title(a)


def test_ltb2():
    """
    Lions and Tigers and Bears
    Suppose we visit a wild animal preserve where we know that the only animals are lions and tigers and bears,
    but we don't know how many of each there are.
    During the tour, we see 3 lions, 2 tigers and one bear.
    Assuming that every animal had an equal chance to appear in our sample, estimate the prevalence of each species.
    What is the probability that the next animal we see is a bear?
    I'll start with a grid algorithm, enumerating the space of prevalences,
    `p1`, `p2`, and `p3`, that add up to 1, and computing the likelihood of the data for each triple of prevalences.
    """
    ps = np.linspace(0, 1, 101)
    suite = LionsTigersBears(enumerate_triples(ps))

    plot_marginal_cdfs(suite)

    for data in "LLLTTB":
        suite.update(data)

    plot_marginal_cdfs(suite)
    suite.marginal(2).mean()
    suite.copy().update("B")

    Dirichlet.Marginal = dirichlet_marginal

    dirichlet = Dirichlet(3)
    plot_marginal_cdfs(dirichlet)

    dirichlet.update((3, 2, 1))

    plot_marginal_pmfs(dirichlet)

    plot_marginal_cdfs(dirichlet)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_marginal_cdfs(suite)

    observed = [0, 0, 0, 1, 1, 2]
    k = len(Pmf(observed))
    a = np.ones(k)

    model = pm.Model()

    with model:
        ps = pm.Dirichlet("ps", a, shape=a.shape)
        xs = pm.Categorical("xs", ps, observed=observed, shape=1)

    logging.info("%r", f"model = {model}")

    with model:
        start = pm.find_MAP()
        step = pm.Metropolis()
        trace = pm.sample(1000, start=start, step=step, tune=100)

    az.plot_trace(trace)

    plot_trace_cdfs(trace)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_trace_cdfs(trace)

    animals = ["lions", "tigers", "bears"]
    c = np.array([3, 2, 1])
    a = np.array([1, 1, 1])

    with pm.Model() as model:
        ps = pm.Dirichlet("ps", a=a, shape=3)  # Probabilities for each species
        xs = pm.Multinomial("xs", n=6, p=ps, shape=3, observed=c)  # multinomial distribution with 6 trials
        trace = pm.sample(draws=100, tune=100)

    pm.traceplot(trace)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    plot_trace_cdfs(trace)

    summary = pm.summary(trace)
    summary.index = animals
    logging.info("%r", f"summary = {summary}")

    ax = pm.plot_posterior(trace, varnames=["ps"])

    for i, a in enumerate(animals):
        ax[i].set_title(a)


def test_ltb4():
    """
    Here’s another Bayes puzzle:
    Suppose we visit a wild animal preserve where we know that the only animals are lions and tigers and bears,
    but we don’t know how many of each there are.
    During the tour, we see 3 lions, 2 tigers, and one bear.
    Assuming that every animal had an equal chance to appear in our sample, estimate the prevalence of each species.
    What is the probability that the next animal we see is a bear?
    """
    n_animals = 3
    with pm.Model() as zoo_model:
        mix = pm.Dirichlet("mix", np.ones(n_animals), shape=n_animals)
        seen_animals = pm.Multinomial(
            "seen_animals",
            n=6,
            p=mix,
            observed=np.array([3, 2, 1])
        )
        next_seen = pm.Multinomial("next_seen", n=1, p=mix, shape=n_animals)
        logging.debug("%r", f"seen_animals={seen_animals}")
        logging.debug("%r", f"next_seen={next_seen}")

        trace = pm.sample(200, tune=100)

    data = az.from_pymc3(
        trace=trace,
        coords={"animal": np.array(["lion", "tiger", "bear"])},
        dims={"mix": ["animal"], "next_seen": ["animal"]},
    )

    az.plot_posterior(data, var_names="mix", round_to=2)

    data.posterior.next_seen.mean(dim=["chain", "draw"]).to_dataframe()


def test_six_species():
    """
    Suppose there are six species that might be in a zoo:
    lions and tigers and bears, and cats and rats and elephants.

    Every zoo has a subset of these species, and every subset is equally likely.
    One day we visit a zoo and see 3 lions, 2 tigers, and one bear.
    Assuming that every animal in the zoo has an equal chance to be seen,
    what is the probability that the next animal we see is an elephant?
    """

    n_animals = 6

    with pm.Model() as zoo_model:
        mix = pm.Dirichlet("mix", np.ones(n_animals), shape=n_animals)
        seen_animals = pm.Multinomial(
            "seen_animals",
            n=6,
            p=mix,
            observed=np.array([3, 2, 1, 0, 0, 0])
        )

        next_seen = pm.Multinomial("next_seen", n=1, p=mix, shape=n_animals)

        logging.debug("%r", f"seen_animals={seen_animals}")
        logging.debug("%r", f"next_seen={next_seen}")
        trace = pm.sample(200, tune=100)

    data = az.from_pymc3(
        trace=trace,
        coords={"animal": ["lion", "tiger", "bear", "cats", "rats", "elephants"]},
        dims={"mix": ["animal"], "next_seen": ["animal"]},
    )

    az.plot_posterior(data, var_names="mix", round_to=2)
    data.posterior.next_seen.mean(dim=["chain", "draw"]).to_dataframe()
