import math
from itertools import product

import numpy as np
from scipy import stats
from scipy.stats import norm

from thinkbayes import Suite, Joint, EvalNormalPdf, Pmf
from thinkbayes.scripts.lincoln import choose, binom


def MakeAngleSuite(data):
    mus = np.linspace(8, 16, 10)
    sigmas = np.linspace(0.1, 2, 10)
    suite = Beetle(product(mus, sigmas))
    suite.Update(data)
    return suite


class Normal(Suite, Joint):
    """
    The `Normal` class provides a `Likelihood` function that computes the likelihood of a sample from a normal distribution.
    """

    def Likelihood(self, data, hypo):
        """

        data: sequence of test scores
        hypo: mu, sigma
        """
        mu, sigma = hypo
        likes = EvalNormalPdf(data, mu, sigma)
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
        pmf.Set(x, prob)
    pmf.Normalize()
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

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: pair of alpha, beta
        data: location of a hit

        Returns: float likelihood
        """
        alpha, beta = hypo
        x = data
        pmf = MakeLocationPmf(alpha, beta, self.locations)
        like = pmf.Prob(x)
        return like


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


def MakeWidthSuite(data):
    mus = np.linspace(115, 160, 10)
    sigmas = np.linspace(1, 10, 10)
    suite = Beetle(product(mus, sigmas))
    suite.Update(data)
    return suite


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


class Classifier(Suite):
    def Likelihood(self, data, hypo):
        return hypo.Likelihood(data)


class Lincoln(Suite, Joint):
    """Represents hypotheses about the number of errors."""

    def Likelihood(self, data, hypo):
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

    def Likelihood(self, data, hypo):
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
