"""
This notebook presents code and exercises from Think Bayes: Chapter 9
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import itertools
import logging
import math
from itertools import product

import numpy as np
import pytest
from scipy import stats
from scipy.stats import norm
from thinkbayes import Pmf, Cdf, Suite, Joint
from thinkbayes import eval_normal_pdf
from thinkbayes import thinkplot
from thinkbayes.scripts import gps
from thinkbayes.scripts.lincoln import choose, binom


def plot_cdfs(df, col):
    for name, group in df.groupby("Species"):
        cdf = Cdf(group[col], label=name)
        thinkplot.plot_cdf_line(cdf)

    thinkplot.config_plot(xlabel=col, legend=True, loc="lower right")


def MakeAngleSuite(data):
    mus = np.linspace(8, 16, 10)
    sigmas = np.linspace(0.1, 2, 10)
    suite = Beetle(product(mus, sigmas))
    suite.update(data)
    return suite


class Normal(Suite, Joint):
    """
    The `Normal` class provides a `Likelihood` function that computes the likelihood of a sample from a normal distribution.
    """

    def likelihood(self, data, hypo):
        """

        data: sequence of test scores
        hypo: mu, sigma
        """
        mu, sigma = hypo
        likes = eval_normal_pdf(data, mu, sigma)
        return np.prod(likes)


def MakeLocationPmf(alpha, beta, locations):
    """Computes the Pmf of the locations, given alpha and beta.

    Given that the shooter is at coordinates (alpha, beta),
    the probability of hitting any spot is inversely proportionate
    to the strafe speed.

    alpha: x position
    beta: y position
    locations: x locations where the pmf is evaluated

    Returns: Pmf object
    """
    pmf = Pmf()
    for x in locations:
        prob = 1.0 / StrafingSpeed(alpha, beta, x)
        pmf.set(x, prob)
    pmf.normalize()
    return pmf


def StrafingSpeed(alpha, beta, x):
    """Computes strafing speed, given location of shooter and impact.

    alpha: x location of shooter
    beta: y location of shooter
    x: location of impact

    Returns: derivative of x with respect to theta
    """
    theta = math.atan2(x - alpha, beta)
    speed = beta / math.cos(theta) ** 2
    return speed


class Paintball(Suite, Joint):
    """
    ## Paintball

    Suppose you are playing paintball in an indoor arena 30 feet
    wide and 50 feet long.  You are standing near one of the 30 foot
    walls, and you suspect that one of your opponents has taken cover
    nearby.  Along the wall, you see several paint spatters, all the same
    color, that you think your opponent fired recently.

    The spatters are at 15, 16, 18, and 21 feet, measured from the
    lower-left corner of the room.  Based on these data, where do you
    think your opponent is hiding?
    Here's the Suite that does the update.  It uses `MakeLocationPmf`,
    defined below.

    Represents hypotheses about the location of an opponent.
    """

    def __init__(self, alphas, betas, locations):
        """Makes a joint suite of parameters alpha and beta.

        Enumerates all pairs of alpha and beta.
        Stores locations for use in Likelihood.

        alphas: possible values for alpha
        betas: possible values for beta
        locations: possible locations along the wall
        """
        self.locations = locations
        pairs = [(alpha, beta) for alpha in alphas for beta in betas]
        Suite.__init__(self, pairs)

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: pair of alpha, beta
        data: location of a hit

        Returns: float likelihood
        """
        alpha, beta = hypo
        x = data
        pmf = MakeLocationPmf(alpha, beta, self.locations)
        like = pmf.prob(x)
        return like


class Beetle(Suite, Joint):
    def likelihood(self, data, hypo):
        """
        data: sequence of measurements
        hypo: mu, sigma
        """
        mu, sigma = hypo
        likes = eval_normal_pdf(data, mu, sigma)
        return np.prod(likes)

    def PredictiveProb(self, data):
        """Compute the posterior total probability of a datum.

        data: sequence of measurements
        """
        total = 0
        for (mu, sigma), prob in self.items():
            likes = norm.pdf(data, mu, sigma)
            total += prob * np.prod(likes)
        return total


def MakeWidthSuite(data):
    mus = np.linspace(115, 160, 10)
    sigmas = np.linspace(1, 10, 10)
    suite = Beetle(product(mus, sigmas))
    suite.update(data)
    return suite


class Species:
    def __init__(self, name, suite_width, suite_angle):
        self.name = name
        self.suite_width = suite_width
        self.suite_angle = suite_angle

    def __str__(self):
        return self.name

    def likelihood(self, data):
        width, angle = data
        like1 = self.suite_width.PredictiveProb(width)
        like2 = self.suite_angle.PredictiveProb(angle)
        return like1 * like2


class Classifier(Suite):
    def likelihood(self, data, hypo):
        return hypo.likelihood(data)


class Lincoln(Suite, Joint):
    """Represents hypotheses about the number of errors."""

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: n, p1, p2
        data: k1, k2, c
        """
        n, p1, p2 = hypo
        k1, k2, c = data

        part1 = choose(n, k1) * binom(k1, n, p1)
        part2 = choose(k1, c) * choose(n - k1, k2 - c) * binom(k2, n, p2)
        return part1 * part2


class Gps(Suite, Joint):
    """Represents hypotheses about your location in the field."""

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo:
        data:
        """
        std = 30
        meanx, meany = hypo
        x, y = data

        like = stats.norm.pdf(x, meanx, std)
        like *= stats.norm.pdf(y, meany, std)
        return like


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
    control.update(data)

    thinkplot.contour_plot(
        control, pcolor_bool=True
    )  # plot the probability of each `mu`-`sigma` pair as a contour plot.
    thinkplot.config_plot(xlabel="mu", ylabel="sigma")

    pmf_mu0 = control.marginal(
        0
    )  # And then we can extract the marginal distribution of `mu`
    thinkplot.plot_pdf_line(pmf_mu0)
    thinkplot.config_plot(xlabel="mu", ylabel="Pmf")

    pmf_sigma0 = control.marginal(1)  # And the marginal distribution of `sigma`
    thinkplot.plot_pdf_line(pmf_sigma0)
    thinkplot.config_plot(xlabel="sigma", ylabel="Pmf")


def test_paintball():
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
    suite.update_set([15, 16, 18, 21])
    locations = range(0, 31)
    alpha = 10
    betas = [10, 20, 40]
    thinkplot.pre_plot(num=len(betas))

    for beta in betas:
        pmf = MakeLocationPmf(alpha, beta, locations)
        pmf.label = f"beta = {beta}"
        thinkplot.plot_pdf_line(pmf)

    thinkplot.config_plot(xlabel="Distance", ylabel="Prob")

    marginal_alpha = suite.marginal(
        0, label="alpha"
    )  # Here are the marginal posterior distributions
    marginal_beta = suite.marginal(1, label="beta")

    print("alpha CI", marginal_alpha.credible_interval(50))
    print("beta CI", marginal_beta.credible_interval(50))

    thinkplot.pre_plot(num=2)

    thinkplot.plot_cdf_line(Cdf(marginal_alpha))
    thinkplot.plot_cdf_line(Cdf(marginal_beta))

    thinkplot.config_plot(xlabel="Distance", ylabel="Prob")

    betas = [10, 20, 40]
    thinkplot.pre_plot(num=len(betas))

    for beta in betas:
        cond = suite.conditional(0, 1, beta)
        cond.label = f"beta = {beta}"
        thinkplot.plot_pdf_line(cond)

    thinkplot.config_plot(xlabel="Distance", ylabel="Prob")

    thinkplot.contour_plot(
        suite.d, contour_bool=False, pcolor_bool=True
    )  # Another way to visualize the posterior distribution

    thinkplot.config_plot(xlabel="alpha", ylabel="beta", axis=[0, 30, 0, 20])

    d = dict((pair, 0) for pair in suite.values())

    percentages = [75, 50, 25]
    for p in percentages:
        interval = suite.max_like_interval(p)
        for pair in interval:
            d[pair] += 1

    thinkplot.contour_plot(d, contour_bool=False, pcolor_bool=True)
    thinkplot.annotate_figure(17, 4, "25", color="white")
    thinkplot.annotate_figure(17, 15, "50", color="white")
    thinkplot.annotate_figure(17, 30, "75")

    thinkplot.config_plot(xlabel="alpha", ylabel="beta", legend=False)


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
    plot_cdfs(df, "Width")
    plot_cdfs(df, "Angle")

    groups = df.groupby("Species")

    for name, group in groups:
        suite = MakeWidthSuite(group.Width)
        thinkplot.contour_plot(suite)
        print(name, suite.PredictiveProb(137))

    for name, group in groups:
        suite = MakeAngleSuite(group.Angle)
        thinkplot.contour_plot(suite)
        print(name, suite.PredictiveProb(13))

    species = {}

    for name, group in groups:
        suite_width = MakeWidthSuite(group.Width)
        suite_angle = MakeAngleSuite(group.Angle)
        species[name] = Species(name, suite_width, suite_angle)

    species["Con"].likelihood((145, 14))

    suite = Classifier(species.values())
    for hypo, prob in suite.items():
        print(hypo, prob)

    suite.update((145, 14))
    for hypo, prob in suite.items():
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
    control.update(data)

    # After the update, we can plot the probability of each `mu`-`sigma` pair as a contour plot.

    thinkplot.contour_plot(control, pcolor_bool=True)
    thinkplot.config_plot(xlabel="mu", ylabel="sigma")

    # And then we can extract the marginal distribution of `mu`

    pmf_mu0 = control.marginal(0)
    thinkplot.plot_pdf_line(pmf_mu0)
    thinkplot.config_plot(xlabel="mu", ylabel="Pmf")

    # And the marginal distribution of `sigma`

    pmf_sigma0 = control.marginal(1)
    thinkplot.plot_pdf_line(pmf_sigma0)
    thinkplot.config_plot(xlabel="sigma", ylabel="Pmf")

    # **Exercise:** Run this analysis again for the control group.
    # What is the distribution of the difference between the groups?
    # What is the probability that the average "reading power" for the treatment group is higher?
    # What is the probability that the variance of the treatment group is higher?

    # Solution

    treated = Normal(itertools.product(mus, sigmas))
    data = df[df.Treatment == "Treated"].Response
    treated.update(data)

    # Solution

    # Here's the posterior joint distribution for the treated group

    thinkplot.contour_plot(treated, pcolor_bool=True)
    thinkplot.config_plot(xlabel="mu", ylabel="Pmf")

    # Solution

    # The marginal distribution of mu

    pmf_mu1 = treated.marginal(0)
    thinkplot.plot_pdf_line(pmf_mu1)
    thinkplot.config_plot(xlabel="mu", ylabel="Pmf")

    # Solution

    # The marginal distribution of sigma

    pmf_sigma1 = treated.marginal(1)
    thinkplot.plot_pdf_line(pmf_sigma1)
    thinkplot.config_plot(xlabel="sigma", ylabel="Pmf")

    # Solution

    # Now we can compute the distribution of the difference between groups

    pmf_diff = pmf_mu1 - pmf_mu0
    logging.info("%r", f"pmf_diff.mean() = {pmf_diff.mean()}")
    logging.info("%r", f"pmf_diff.map() = {pmf_diff.map()}")

    # Solution

    # And CDF_diff(0), which is the probability that the difference is <= 0

    pmf_diff = pmf_mu1 - pmf_mu0
    cdf_diff = pmf_diff.make_cdf()
    thinkplot.plot_cdf_line(cdf_diff)
    logging.info("%r", f"cdf_diff[0] = {cdf_diff[0]}")

    # Solution

    # Or we could directly compute the probability that mu is
    # greater than mu2

    pmf_mu1.prob_greater(pmf_mu0)

    # Solution

    # Finally, here's the probability that the standard deviation
    # in the treatment group is higher.

    pmf_sigma1.prob_greater(pmf_sigma0)

    # It looks like there is a high probability that the mean of
    # the treatment group is higher, and the most likely size of
    # the effect is 9-10 points.

    # It looks like the variance of the treated group is substantially
    # smaller, which suggests that the treatment might be helping
    # low scorers more than high scorers.


def test_paintballing():
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
    suite.update_set([15, 16, 18, 21])

    # To visualize the joint posterior,
    # I take slices for a few values of `beta` and plot the conditional distributions of `alpha`.
    # If the shooter is close to the wall, we can be somewhat confident of his position.
    # The farther away he is, the less certain we are.

    locations = range(0, 31)
    alpha = 10
    betas = [10, 20, 40]
    thinkplot.pre_plot(num=len(betas))

    for beta in betas:
        pmf = MakeLocationPmf(alpha, beta, locations)
        pmf.label = f"beta = {beta}"
        thinkplot.plot_pdf_line(pmf)

    thinkplot.config_plot(xlabel="Distance", ylabel="Prob")

    # Here are the marginal posterior distributions for `alpha` and `beta`.

    marginal_alpha = suite.marginal(0, label="alpha")
    marginal_beta = suite.marginal(1, label="beta")

    print("alpha CI", marginal_alpha.credible_interval(50))
    print("beta CI", marginal_beta.credible_interval(50))

    thinkplot.pre_plot(num=2)

    thinkplot.plot_cdf_line(Cdf(marginal_alpha))
    thinkplot.plot_cdf_line(Cdf(marginal_beta))

    thinkplot.config_plot(xlabel="Distance", ylabel="Prob")

    # To visualize the joint posterior, I take slices for a few values of `beta` and
    # plot the conditional distributions of `alpha`.
    # If the shooter is close to the wall, we can be somewhat confident of his position.
    # The farther away he is, the less certain we are.

    betas = [10, 20, 40]
    thinkplot.pre_plot(num=len(betas))

    for beta in betas:
        cond = suite.conditional(0, 1, beta)
        cond.label = f"beta = {beta}"
        thinkplot.plot_pdf_line(cond)

    thinkplot.config_plot(xlabel="Distance", ylabel="Prob")

    # Another way to visualize the posterio distribution:
    # a pseudocolor plot of probability as a function of `alpha` and `beta`.

    thinkplot.contour_plot(suite.d, contour_bool=False, pcolor_bool=True)

    thinkplot.config_plot(xlabel="alpha", ylabel="beta", axis=[0, 30, 0, 20])

    # Here's another visualization that shows posterior credible regions.

    d = dict((pair, 0) for pair in suite.values())

    percentages = [75, 50, 25]
    for p in percentages:
        interval = suite.max_like_interval(p)
        for pair in interval:
            d[pair] += 1

    thinkplot.contour_plot(d, contour_bool=False, pcolor_bool=True)
    thinkplot.annotate_figure(17, 4, "25", color="white")
    thinkplot.annotate_figure(17, 15, "50", color="white")
    thinkplot.annotate_figure(17, 30, "75")

    thinkplot.config_plot(xlabel="alpha", ylabel="beta", legend=False)


@pytest.mark.skip(reason="long running")
def test_bugs():
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
    suite.update(data)

    # Solution

    n_marginal = suite.marginal(0)
    thinkplot.plot_pmf_line(n_marginal, label="n")
    thinkplot.config_plot(xlabel="number of bugs", ylabel="PMF")

    # Solution

    print("post mean n", n_marginal.mean())
    print("MAP n", n_marginal.map())


def test_gps():
    gps.main()
