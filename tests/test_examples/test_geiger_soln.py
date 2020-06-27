"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np
import pandas as pd
from thinkbayes import make_poisson_pmf, eval_binomial_pmf, make_mixture
from thinkbayes import Pmf, Cdf, Suite, Joint
from thinkbayes import thinkplot
from thinkbayes.thinkplot import POSTERIOR_MARGINAL_LABEL


def test_geiger_counter_problem():
    # ## The Geiger counter problem
    #
    # I got the idea for the following problem from Tom Campbell-Ricketts, author of the [Maximum Entropy blog](http://maximum-entropy-blog.blogspot.com). And he got the idea from E. T. Jaynes, author of the classic *Probability Theory: The Logic of Science*:
    #
    # > Suppose that a radioactive source emits particles toward a Geiger counter at an average rate of r particles per second, but the counter only registers a fraction, f, of the particles that hit it. If f is 10% and the counter registers 15 particles in a one second interval, what is the posterior distribution of n, the actual number of particles that hit the counter, and r, the average rate particles are emitted?
    #
    #
    #
    #

    # ### Grid algorithm
    #
    #

    class Logistic(Suite, Joint):
        def likelihood(self, data, hypo):
            """

            data: k, number of particles detected
            hypo: r, emission rate in particles per second
            """
            return 1

    r = 160
    k = 15
    f = 0.1

    pmf = make_poisson_pmf(r, high=500)
    thinkplot.plot_hist_bar(pmf)

    total = 0
    for n, p in pmf.items():
        total += p * eval_binomial_pmf(k, n, f)

    logging.info("%r", f"total = {total}")


    def compute_likelihood(k, r, f):
        pmf = make_poisson_pmf(r, high=500)
        total = 0
        for n, p in pmf.items():
            total += p * eval_binomial_pmf(k, n, f)

        return total

    compute_likelihood(k, r, f)

    likes = pd.Series([])
    for kk in range(0, 40):
        likes[kk] = compute_likelihood(kk, r, f)

    likes.plot()
    thinkplot.decorate(xlabel="Counter particles (n)", ylabel="PMF")

    # Solution

    class Logistic(Suite, Joint):
        f = 0.1

        def likelihood(self, data, hypo):
            """

            data: k, number of particles detected
            hypo: r, emission rate in particles per second

            """
            k = data
            r = hypo
            return compute_likelihood(k, r, self.f)

    rs = np.linspace(0, 300, 51)

    suite = Logistic(rs)

    suite.update(15)

    thinkplot.plot_pdf_line(suite)
    thinkplot.decorate(
        xlabel="Emission rate (particles/second)",
        ylabel="PMF",
        title=POSTERIOR_MARGINAL_LABEL,
    )

    # ### MCMC
    #
    # Implement this model using MCMC.  As a starting place, you can use this example from [the PyMC3 docs](https://docs.pymc.io/notebooks/GLM-logistic.html#The-model).
    #
    # As a challege, try writing the model more explicitly, rather than using the GLM module.

    import pymc3 as pm

    # Solution

    f = 0.1
    model = pm.Model()

    with model:
        r = pm.Uniform("r", 0, 500)
        n = pm.Poisson("n", r)
        k = pm.Binomial("k", n, f, observed=15)
        trace = pm.sample_prior_predictive(1000)

    thinkplot.plot_cdf_line(Cdf(trace["r"]))

    thinkplot.plot_cdf_line(Cdf(trace["n"]))

    thinkplot.plot_cdf_line(Cdf(trace["k"]))

    with model:
        trace = pm.sample(1000, tune=3000)

    pm.traceplot(trace)

    n_sample = trace["n"]
    thinkplot.plot_cdf_line(Cdf(n_sample))

    r_sample = trace["r"]
    thinkplot.plot_cdf_line(Cdf(r_sample))

    thinkplot.plot_cdf_line(suite.make_cdf())
    thinkplot.plot_cdf_line(Cdf(r_sample))

    # ### Grid algorithm, version 2

    # Solution

    class Logistic(Suite, Joint):
        f = 0.1

        def likelihood(self, data, hypo):
            """

            data: k, number of particles detected
            hypo: r, n
            """
            k = data
            r, n = hypo
            return eval_binomial_pmf(k, n, self.f)

    rs = np.linspace(0, 300, 51)

    suite = Logistic()

    for r in rs:
        pmf = make_poisson_pmf(r, high=500)
        for n, p in pmf.items():
            suite[r, n] += p

    suite.normalize()

    suite.update(15)

    pmf_r = suite.marginal(0)
    thinkplot.plot_pdf_line(pmf_r)
    thinkplot.decorate(
        xlabel="Emission rate (particles/second)",
        ylabel="PMF",
        title=POSTERIOR_MARGINAL_LABEL,
    )

    pmf_n = suite.marginal(1)
    thinkplot.plot_pdf_line(pmf_n)
    thinkplot.decorate(
        xlabel="Number of particles (n)",
        ylabel="PMF",
        title=POSTERIOR_MARGINAL_LABEL,
    )

    # ### Hierarchical version, as in the book

    class Detector(Suite):
        """Represents hypotheses about n."""

        def __init__(self, r, f, high=500):
            """Initializes the suite.

            r: known emission rate, r
            f: fraction of particles registered
            high: maximum number of particles, n
            """
            pmf = make_poisson_pmf(r, high)
            super().__init__(pmf)
            self.r = r
            self.f = f

        def likelihood(self, data, hypo):
            """Likelihood of the data given the hypothesis.

            data: number of particles counted
            hypo: number of particles hitting the counter, n
            """
            k = data
            n = hypo

            return eval_binomial_pmf(k, n, self.f)

    r = 160
    k = 15
    f = 0.1

    suite = Detector(r, f)

    suite.update(15)

    class Emitter(Suite):
        """Represents hypotheses about r."""

        def likelihood(self, data, hypo):
            """Likelihood of the data given the hypothesis.

            data: number of counted per unit time
            hypo: Detector object
            """
            return hypo.update(data)

    rs = np.linspace(0, 300, 51)

    detectors = [Detector(r, f=0.1) for r in rs[1:]]
    suite = Emitter(detectors)

    suite.update(15)

    pmf_r = Pmf()
    for detector, p in suite.items():
        pmf_r[detector.r] = p

    thinkplot.plot_pdf_line(pmf_r)

    mix = make_mixture(suite)

    thinkplot.plot_pdf_line(mix)
