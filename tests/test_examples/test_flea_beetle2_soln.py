"""
Think Bayes
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import math
import numpy as np

from thinkbayes import Pmf, Cdf, Suite, Joint
from thinkbayes import thinkplot


def test_flea_beetle_problem():
    # ### The flea beetle problem
    #
    # Different species of flea beetle can be distinguished by the width and angle of the aedeagus. The data below includes measurements and know species classification for 74 specimens.
    #
    # Suppose you discover a new specimen under conditions where it is equally likely to be any of the three species. You measure the aedeagus and width 140 microns and angle 15 (in multiples of 7.5 degrees). What is the probability that it belongs to each species?
    #
    # This problem is based on [this data story on DASL](https://web.archive.org/web/20160304083805/http://lib.stat.cmu.edu/DASL/Datafiles/FleaBeetles.html)
    #
    # Datafile Name: Flea Beetles
    #
    # Datafile Subjects: Biology
    #
    # Story Names: Flea Beetles
    #
    # Reference: Lubischew, A.A. (1962) On the use of discriminant functions in taxonomy. Biometrics, 18, 455-477. Also found in: Hand, D.J., et al. (1994) A Handbook of Small Data Sets, London: Chapman & Hall, 254-255.
    #
    # Authorization: Contact Authors
    #
    # Description: Data were collected on the genus of flea beetle Chaetocnema, which contains three species: concinna (Con), heikertingeri (Hei), and heptapotamica (Hep). Measurements were made on the width and angle of the aedeagus of each beetle. The goal of the original study was to form a classification rule to distinguish the three species.
    #
    # Number of cases: 74
    #
    # Variable Names:
    #
    # Width: The maximal width of aedeagus in the forpart (in microns)
    #
    # Angle: The front angle of the aedeagus (1 unit = 7.5 degrees)
    #
    # Species: Species of flea beetle from the genus Chaetocnema
    #

    # To solve this problem we have to account for two sources of uncertainty: given the data, we have some uncertainty about the actual distribution of attributes.  Then, given the measurements, we have uncertainty about which species we have.
    #
    # First I'll load the data.

    measurements = (140, 15)

    import pandas as pd

    df = pd.read_csv("../data/flea_beetles.csv", delimiter="\t")
    df.head()

    def plot_cdfs(df, col):
        for name, group in df.groupby("Species"):
            cdf = Cdf(group[col], label=name)
            thinkplot.Cdf(cdf)

        thinkplot.Config(xlabel=col, legend=True, loc="lower right")

    plot_cdfs(df, "Width")

    plot_cdfs(df, "Angle")

    # The following class estimates the mean and standard deviation of a normal distribution, given the data:

    from scipy.stats import norm
    from thinkbayes import EvalNormalPdf

    class Beetle(Suite, Joint):
        def Likelihood(self, data, hypo):
            """
            data: sequence of measurements
            hypo: mu, sigma
            """
            mu, sigma = hypo
            likes = EvalNormalPdf(data, mu, sigma)
            return np.prod(likes)

        def PredictiveProb(self, data):
            """Compute the posterior total probability of a datum.

            data: sequence of measurements
            """
            total = 0
            for (mu, sigma), prob in self.Items():
                likes = norm.pdf(data, mu, sigma)
                total += prob * np.prod(likes)
            return total

    # Now we can estimate parameters for the widths, for each of the three species.

    from itertools import product

    def MakeWidthSuite(data):
        mus = np.linspace(115, 160, 51)
        sigmas = np.linspace(1, 10, 51)
        suite = Beetle(product(mus, sigmas))
        suite.Update(data)
        return suite

    groups = df.groupby("Species")

    # Here are the posterior distributions for mu and sigma, and the predictive probability of the width measurement, for each species.

    for name, group in groups:
        suite = MakeWidthSuite(group.Width)
        thinkplot.Contour(suite)
        print(name, suite.PredictiveProb(140))

    # Now we can do the same thing for the angles.

    def MakeAngleSuite(data):
        mus = np.linspace(8, 16, 101)
        sigmas = np.linspace(0.1, 2, 101)
        suite = Beetle(product(mus, sigmas))
        suite.Update(data)
        return suite

    for name, group in groups:
        suite = MakeAngleSuite(group.Angle)
        thinkplot.Contour(suite)
        print(name, suite.PredictiveProb(15))

    # These posterior distributions are used to compute the likelihoods of the measurements.

    class Species:
        def __init__(self, name, suite_width, suite_angle):
            self.name = name
            self.suite_width = suite_width
            self.suite_angle = suite_angle

        def __str__(self):
            return self.name

        def Likelihood(self, data):
            width, angle = data
            like1 = self.suite_width.PredictiveProb(width)
            like2 = self.suite_angle.PredictiveProb(angle)
            return like1 * like2

    species = {}

    for name, group in groups:
        suite_width = MakeWidthSuite(group.Width)
        suite_angle = MakeAngleSuite(group.Angle)
        species[name] = Species(name, suite_width, suite_angle)

    # For example, here's the likelihood of the data given that the species is 'Con'

    species["Con"].Likelihood(measurements)

    # Now we can make a `Classifier` that uses the `Species` objects as hypotheses.

    class Classifier(Suite):
        def Likelihood(self, data, hypo):
            return hypo.Likelihood(data)

    suite = Classifier(species.values())
    for hypo, prob in suite.Items():
        print(hypo, prob)

    suite.Update(measurements)
    for hypo, prob in suite.Items():
        print(hypo, prob)

    # ## Now with MCMC
    #
    # Based on [this example](https://docs.pymc.io/notebooks/LKJ.html)

    from warnings import simplefilter

    simplefilter("ignore", FutureWarning)

    import pymc3 as pm

    N = 10000

    μ_actual = np.array([1, -2])
    Σ_actual = np.array([[0.5, -0.3], [-0.3, 1.0]])

    x = np.random.multivariate_normal(μ_actual, Σ_actual, size=N)

    df["Width10"] = df.Width / 10

    observed = {}
    for name, group in df.groupby("Species"):
        observed[name] = group[["Width10", "Angle"]].values
        print(name)
        print(np.cov(np.transpose(observed[name])))

    x = observed["Con"]

    with pm.Model() as model:
        packed_L = pm.LKJCholeskyCov(
            "packed_L", n=2, eta=2, sd_dist=pm.HalfCauchy.dist(2.5)
        )

    with model:
        L = pm.expand_packed_triangular(2, packed_L)
        Σ = pm.Deterministic("Σ", L.dot(L.T))

    with model:
        μ = pm.Normal("μ", 0.0, 10.0, shape=2, testval=x.mean(axis=0))
        obs = pm.MvNormal("obs", μ, chol=L, observed=x)

    with model:
        trace = pm.sample(1000)

    pm.traceplot(trace)

    μ_post = trace["μ"].mean(axis=0)

    Σ_post = trace["Σ"].mean(axis=0)

    from statsmodels.stats.moment_helpers import cov2corr

    from scipy.stats import multivariate_normal

    cov2corr(Σ_post)

    measured = (14, 15)

    total = 0
    for row in trace:
        total += multivariate_normal.pdf(measured, mean=row["μ"], cov=row["Σ"])

    total / len(trace)

    def compute_posterior_likelihood(measured, species):
        x = observed[species]

        with pm.Model() as model:
            packed_L = pm.LKJCholeskyCov(
                "packed_L", n=2, eta=2, sd_dist=pm.HalfCauchy.dist(2.5)
            )
            L = pm.expand_packed_triangular(2, packed_L)
            Σ = pm.Deterministic("Σ", L.dot(L.T))
            μ = pm.Normal("μ", 0.0, 10.0, shape=2, testval=x.mean(axis=0))
            obs = pm.MvNormal("obs", μ, chol=L, observed=x)
            trace = pm.sample(1000)

        total = 0
        for row in trace:
            total += multivariate_normal.pdf(measured, mean=row["μ"], cov=row["Σ"])

        return total / len(trace)

    suite = Suite(["Con", "Hep", "Hei"])

    for hypo in suite:
        like = compute_posterior_likelihood(measured, hypo)
        print(hypo, like)
        suite[hypo] *= like

    suite.Normalize()

    suite.Print()
