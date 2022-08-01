"""
This notebook presents code and exercises from Think Bayes: Chapter 9
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import itertools
import logging
from itertools import product

import numpy as np
import pytest

import thinkplot
from thinkbayes.c09_decisions import (
    MakeAngleSuite,
    Normal,
    MakeLocationPmf,
    Paintball,
    MakeWidthSuite,
    Species,
    Classifier,
    Lincoln
)
from thinkbayes.scripts import gps


def test_reading(drp_scores_df):
    """

    Improving Reading Ability From DASL(http://lib.stat.cmu.edu/DASL/Stories/ImprovingReadingAbility.html)

    An educator conducted an experiment to test whether new directed reading activities in the classroom
    will help elementary school pupils improve some aspects of their reading ability.
    She arranged for a third grade class of 21 students to follow these activities for an 8-week period.
    A control classroom of 23 third graders followed the same curriculum without the activities.
    At the end of the 8 weeks, all students took a Degree of Reading Power (DRP) test,
    which measures the aspects of reading ability that the treatment is designed to improve.

    Summary statistics on the two groups of children show that the average score of the treatment class was
    almost ten points higher than the average of the control class.
    A two-sample t-test is appropriate for testing whether this difference is statistically significant.
    The t-statistic is 2.31, which is significant at the .05 level.

    use `groupby` to compute the means for the two groups.

    It looks like there is a high probability that the mean of
    the treatment group is higher, and the most likely size of
    the effect is 9-10 points.

    It looks like the variance of the treated group is substantially
    smaller, which suggests that the treatment might be helping
    low scorers more than high scorers.

    :return:
    """
    df = drp_scores_df
    grouped = df.groupby("Treatment")
    for name, group in grouped:
        print(name, group.Response.mean())

    mus = np.linspace(
        20, 80, 10
    )  # The prior distributions for `mu` and `sigma` are uniform.
    sigmas = np.linspace(5, 30, 10)
    control = Normal(product(mus, sigmas))
    data = df[df.Treatment == "Control"].Response
    control.Update(data)

    pmf_mu0 = control.Marginal(
        0
    )  # And then we can extract the marginal distribution of `mu`


def paintball_strategy(self, body: dict):
    """
    The prior probabilities for `alpha` and `beta` are uniform.

    To visualize the joint posterior, I take slices for a few values of `beta` and
    plot the conditional distributions of `alpha`.
    If the shooter is close to the wall, we can be somewhat confident of his position.
    The farther away he is, the less certain we are.

    To visualize the joint posterior, I take slices for a few values of `beta` and
    plot the conditional distributions of `alpha`.
    If the shooter is close to the wall, we can be somewhat confident of his position.
    The farther away he is, the less certain we are.

    :return:
    """
    alphas = range(0, 31)
    betas = range(1, 51)
    locations = range(0, 31)

    suite = Paintball(alphas, betas, locations)
    suite.UpdateSet([15, 16, 18, 21])
    locations = range(0, 31)
    alpha = 10
    betas = [10, 20, 40]
    thinkplot.PrePlot(num=len(betas))

    marginal_alpha = suite.Marginal(
        0, label="alpha"
    )  # Here are the marginal posterior distributions
    marginal_beta = suite.Marginal(1, label="beta")

    print("alpha CI", marginal_alpha.CredibleInterval(50))
    print("beta CI", marginal_beta.CredibleInterval(50))

    betas = [10, 20, 40]
    thinkplot.PrePlot(num=len(betas))

    d = dict((pair, 0) for pair in suite.Values())

    percentages = [75, 50, 25]
    for p in percentages:
        interval = suite.MaxLikeInterval(p)
        for pair in interval:
            d[pair] += 1


def test_flea_beetles(flea_beetles_df):
    # **Exercise:** [The Flea Beetle problem from DASL](http://lib.stat.cmu.edu/DASL/Datafiles/FleaBeetles.html)
    # Datafile Name: Flea Beetles
    # Datafile Subjects: Biology
    # Story Names: Flea Beetles
    # Reference: Lubischew, A.A. (1962) On the use of discriminant functions in taxonomy. Biometrics, 18, 455-477.
    # Also found in: Hand, D.J., et al. (1994) A Handbook of Small Data Sets, London: Chapman & Hall, 254-255.
    # Authorization: Contact Authors
    # Description: Data were collected on the genus of flea beetle Chaetocnema,
    # which contains three species: concinna (Con), heikertingeri (Hei), and heptapotamica (Hep).
    # Measurements were made on the width and angle of the aedeagus of each beetle.
    # The goal of the original study was to form a classification rule to distinguish the three species.
    # Number of cases: 74
    # Variable Names:
    # Width: The maximal width of aedeagus in the forpart (in microns)
    # Angle: The front angle of the aedeagus (1 unit = 7.5 degrees)
    # Species: Species of flea beetle from the genus Chaetocnema

    # Suggestions:
    # 1. Plot CDFs for the width and angle data, broken down by species,
    # to get a visual sense of whether the normal distribution is a good model.
    # 2. Use the data to estimate the mean and standard deviation for each variable, broken down by species.
    # 3. Given a joint posterior distribution for `mu` and `sigma`, what is the likelihood of a given datum?
    # 4. Write a function that takes a measured width and angle and returns a posterior PMF of species.
    # 5. Use the function to classify each of the specimens in the table and see how many you get right.

    df = flea_beetles_df

    groups = df.groupby("Species")

    for name, group in groups:
        suite = MakeWidthSuite(group.Width)
        print(name, suite.PredictiveProb(137))

    for name, group in groups:
        suite = MakeAngleSuite(group.Angle)
        print(name, suite.PredictiveProb(13))

    species = {}

    for name, group in groups:
        suite_width = MakeWidthSuite(group.Width)
        suite_angle = MakeAngleSuite(group.Angle)
        species[name] = Species(name, suite_width, suite_angle)

    species["Con"].Likelihood((145, 14))

    suite = Classifier(species.values())
    for hypo, prob in suite.Items():
        print(hypo, prob)

    suite.Update((145, 14))
    for hypo, prob in suite.Items():
        print(hypo, prob)


def test_improving_reading_ability(drp_scores_df):
    # ## Improving Reading Ability
    # From DASL(http://lib.stat.cmu.edu/DASL/Stories/ImprovingReadingAbility.html)
    # > An educator conducted an experiment to test whether new directed reading activities in the classroom
    # will help elementary school pupils improve some aspects of their reading ability.
    # She arranged for a third grade class of 21 students to follow these activities for an 8-week period.
    # A control classroom of 23 third graders followed the same curriculum without the activities.
    # At the end of the 8 weeks, all students took a Degree of Reading Power (DRP) test,
    # which measures the aspects of reading ability that the treatment is designed to improve.
    # > Summary statistics on the two groups of children show that the average score of the treatment class
    # was almost ten points higher than the average of the control class.
    # A two-sample t-test is appropriate for testing whether this difference is statistically significant.
    # The t-statistic is 2.31, which is significant at the .05 level.

    df = drp_scores_df
    grouped = df.groupby("Treatment")
    for name, group in grouped:
        print(name, group.Response.mean())

    # The `Normal` class provides a `Likelihood` function that
    # computes the likelihood of a sample from a normal distribution.
    # The prior distributions for `mu` and `sigma` are uniform.

    mus = np.linspace(20, 80, 10)
    sigmas = np.linspace(5, 30, 10)

    # I use `itertools.product` to enumerate all pairs of `mu` and `sigma`.

    control = Normal(itertools.product(mus, sigmas))
    data = df[df.Treatment == "Control"].Response
    control.Update(data)

    # After the update, we can plot the probability of each `mu`-`sigma` pair as a contour plot.

    # And then we can extract the marginal distribution of `mu`

    pmf_mu0 = control.Marginal(0)

    # And the marginal distribution of `sigma`

    pmf_sigma0 = control.Marginal(1)

    # **Exercise:** Run this analysis again for the control group.
    # What is the distribution of the difference between the groups?
    # What is the probability that the average "reading power" for the treatment group is higher?
    # What is the probability that the variance of the treatment group is higher?

    # Solution

    treated = Normal(itertools.product(mus, sigmas))
    data = df[df.Treatment == "Treated"].Response
    treated.Update(data)

    # Solution

    # Here's the posterior joint distribution for the treated group

    # Solution

    # The marginal distribution of mu

    pmf_mu1 = treated.Marginal(0)

    # Solution

    # The marginal distribution of sigma

    pmf_sigma1 = treated.Marginal(1)

    # Solution

    # Now we can compute the distribution of the difference between groups

    pmf_diff = pmf_mu1 - pmf_mu0
    logging.info("%r", f"pmf_diff.mean() = {pmf_diff.Mean()}")
    logging.info("%r", f"pmf_diff.map() = {pmf_diff.MAP()}")

    # Solution

    # And CDF_diff(0), which is the probability that the difference is <= 0

    pmf_diff = pmf_mu1 - pmf_mu0
    cdf_diff = pmf_diff.MakeCdf()

    logging.info("%r", f"cdf_diff[0] = {cdf_diff[0]}")

    # Solution

    # Or we could directly compute the probability that mu is
    # greater than mu2

    pmf_mu1.ProbGreater(pmf_mu0)

    # Solution

    # Finally, here's the probability that the standard deviation
    # in the treatment group is higher.

    pmf_sigma1.ProbGreater(pmf_sigma0)

    # It looks like there is a high probability that the mean of
    # the treatment group is higher, and the most likely size of
    # the effect is 9-10 points.

    # It looks like the variance of the treated group is substantially
    # smaller, which suggests that the treatment might be helping
    # low scorers more than high scorers.


def paintballing_strategy(self, body: dict):
    # ## Paintball

    # Suppose you are playing paintball in an indoor arena 30 feet
    # wide and 50 feet long.  You are standing near one of the 30 foot
    # walls, and you suspect that one of your opponents has taken cover
    # nearby.  Along the wall, you see several paint spatters, all the same
    # color, that you think your opponent fired recently.
    #
    # The spatters are at 15, 16, 18, and 21 feet, measured from the
    # lower-left corner of the room.  Based on these data, where do you
    # think your opponent is hiding?
    #
    # Here's the Suite that does the update.  It uses `MakeLocationPmf`,
    # defined below.

    # The prior probabilities for `alpha` and `beta` are uniform.

    alphas = range(0, 31)
    betas = range(1, 51)
    locations = range(0, 31)

    suite = Paintball(alphas, betas, locations)
    suite.UpdateSet([15, 16, 18, 21])

    # To visualize the joint posterior,
    # I take slices for a few values of `beta` and plot the conditional distributions of `alpha`.
    # If the shooter is close to the wall, we can be somewhat confident of his position.
    # The farther away he is, the less certain we are.

    locations = range(0, 31)
    alpha = 10
    betas = [10, 20, 40]
    thinkplot.PrePlot(num=len(betas))

    for beta in betas:
        pmf = MakeLocationPmf(alpha, beta, locations)
        pmf.label = f"beta = {beta}"

    # Here are the marginal posterior distributions for `alpha` and `beta`.

    marginal_alpha = suite.Marginal(0, label="alpha")
    marginal_beta = suite.Marginal(1, label="beta")

    print("alpha CI", marginal_alpha.CredibleInterval(50))
    print("beta CI", marginal_beta.CredibleInterval(50))

    # To visualize the joint posterior, I take slices for a few values of `beta` and
    # plot the conditional distributions of `alpha`.
    # If the shooter is close to the wall, we can be somewhat confident of his position.
    # The farther away he is, the less certain we are.

    betas = [10, 20, 40]

    for beta in betas:
        cond = suite.Conditional(0, 1, beta)
        cond.label = f"beta = {beta}"

    # Another way to visualize the posterio distribution:
    # a pseudocolor plot of probability as a function of `alpha` and `beta`.

    # Here's another visualization that shows posterior credible regions.

    d = dict((pair, 0) for pair in suite.Values())

    percentages = [75, 50, 25]
    for p in percentages:
        interval = suite.MaxLikeInterval(p)
        for pair in interval:
            d[pair] += 1


@pytest.mark.skip(reason="long running")
def bugs_strategy(self, body: dict):
    # **Exercise:** From [John D. Cook](http://www.johndcook.com/blog/2010/07/13/lincoln-index/)
    # "Suppose you have a tester who finds 20 bugs in your program.
    # You want to estimate how many bugs are really in the program.
    # You know there are at least 20 bugs, and if you have supreme confidence in your tester,
    # you may suppose there are around 20 bugs.
    # But maybe your tester isn't very good.
    # Maybe there are hundreds of bugs.
    # How can you have any idea how many bugs there are?
    # There’s no way to know with one tester.
    # But if you have two testers, you can get a good idea, even if you don’t know how skilled the testers are.
    #
    # Suppose two testers independently search for bugs.
    # Let k1 be the number of errors the first tester finds and k2 the number of errors the second tester finds.
    # Let c be the number of errors both testers find.
    # The Lincoln Index estimates the
    # total number of errors as k1 k2 / c [I changed his notation to be consistent with mine]."
    # So if the first tester finds 20 bugs, the second finds 15, and they find 3 in common,
    # we estimate that there are about 100 bugs.
    # What is the Bayesian estimate of the number of errors based on this data?

    # Solution

    # Solution

    data = 20, 15, 3
    probs = np.linspace(0, 1, 3)
    hypos = []
    for n in np.linspace(75, 150, 5):
        for p1 in probs:
            for p2 in probs:
                hypos.append((n, p1, p2))

    suite = Lincoln(hypos)
    suite.Update(data)

    # Solution

    n_marginal = suite.Marginal(0)

    # Solution

    print("post mean n", n_marginal.mean())
    print("MAP n", n_marginal.map())


@pytest.mark.skip(reason="long running test")
def gps_strategy(self, body: dict):
    gps.main()
