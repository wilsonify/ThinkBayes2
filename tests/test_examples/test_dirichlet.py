"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from itertools import product

import numpy as np
import pytest
from thinkbayes import Dirichlet
from thinkbayes import Pmf, Cdf, Suite, Joint
from thinkbayes import thinkplot


def DirichletMarginal(dirichlet, i):
    return dirichlet.MarginalBeta(i).MakePmf()


Dirichlet.Marginal = DirichletMarginal


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

    def Likelihood(self, data, hypo):
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


def plot_marginal_cdfs(joint):
    pmf_lion = joint.Marginal(0)
    pmf_tiger = joint.Marginal(1)
    pmf_bear = joint.Marginal(2)

    thinkplot.plot_cdf_line(pmf_lion.MakeCdf(), label="lions")
    thinkplot.plot_cdf_line(pmf_tiger.MakeCdf(), label="tigers")
    thinkplot.plot_cdf_line(pmf_bear.MakeCdf(), label="bears")

    thinkplot.decorate(xlabel="Prevalence", ylabel="CDF")


def plot_marginal_pmfs(joint):
    pmf_lion = joint.Marginal(0)
    pmf_tiger = joint.Marginal(1)
    pmf_bear = joint.Marginal(2)

    thinkplot.plot_pdf_line(pmf_lion, label="lions")
    thinkplot.plot_pdf_line(pmf_tiger, label="tigers")
    thinkplot.plot_pdf_line(pmf_bear, label="bears")

    thinkplot.decorate(xlabel="Prevalence", ylabel="PMF")


@pytest.fixture(name="suite")
def suite_fixture():
    ps = np.linspace(0, 1, 101)
    ps = np.round(ps, 2)

    def enumerate_triples(ps):
        for p1, p2, p3 in product(ps, ps, ps):
            if p1 + p2 + p3 == 1:
                yield p1, p2, p3

    suite = LionsTigersBears(enumerate_triples(ps))
    return suite


def test_ltb(suite):
    # Solution goes here

    plot_marginal_pmfs(suite)

    for data in "LLLTTB":
        print(data)
        suite.Update([data])

    plot_marginal_pmfs(suite)

    plot_marginal_cdfs(suite)


def test_dirichlet(suite):
    # ### Using the Dirichlet object

    dirichlet = Dirichlet(3)
    plot_marginal_pmfs(dirichlet)

    dirichlet.Update((3, 2, 1))

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
    import pymc3 as pm

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
        trace = dict(ps=pm.sample(200, step))
        # a = pm.traceplot(trace)

    def plot_trace_cdfs(trace):
        rows = np.asarray(trace).transpose()

        cdf_lion = Cdf(rows[0])
        cdf_tiger = Cdf(rows[1])
        cdf_bear = Cdf(rows[2])

        thinkplot.plot_cdf_line(cdf_lion, label="lions")
        thinkplot.plot_cdf_line(cdf_tiger, label="tigers")
        thinkplot.plot_cdf_line(cdf_bear, label="bears")

        thinkplot.decorate(xlabel="Prevalence", ylabel="CDF")

    # plot_trace_cdfs(trace)

    # pmf = Pmf(trace['xs'][0])
    # thinkplot.Hist(pmf)

    with model:
        start = pm.find_MAP()
        step = pm.Metropolis()
        trace = pm.sample(1000, start=start, step=step, tune=1000)

    # pm.traceplot(trace)

    # plot_trace_cdfs(trace)

    thinkplot.pre_plot(6)
    plot_marginal_cdfs(dirichlet)
    # plot_trace_cdfs(trace)
