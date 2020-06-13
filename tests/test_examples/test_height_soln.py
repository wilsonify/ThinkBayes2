"""
Think Bayes
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy as np
import pandas as pd
import pytest

from thinkbayes import Pmf, Suite, Joint
from thinkbayes import thinkplot
from thinkbayes import MakeMixture
from scipy.stats import norm


class Height(Suite):
    """
    The height problem
    For adult male residents of the US, the mean and standard deviation of height are 178 cm and 7.7 cm.
    For adult female residents the corresponding stats are 163 cm and 7.3 cm.
    Suppose you learn that someone is 170 cm tall.
    What is the probability that they are male?

    Run this analysis again for a range of observed heights and plot a curve that shows P(male) versus height.
    What is the mathematical form of this function?
    To represent the likelihood functions,
    I'll use `norm` from `scipy.stats`, which returns a "frozen" random variable (RV)
    that represents a normal distribution with given parameters.
    """

    def Likelihood(self, data, hypo):
        """
        data: height in cm
        hypo: 'male' or 'female'
        """
        self.dist_height = dict(male=norm(178, 7.7), female=norm(163, 7.3))
        height = data
        return self.dist_height[hypo].pdf(height)

    def prob_male(self, height):
        """
        Compute the probability of being male as a function of height, for a range of values between 150 and 200.
        """
        self.Update(height)
        return self["male"]


def test_Height():
    suite = Height(dict(male=0.49, female=0.51))
    for hypo, prob in suite.Items():
        print(hypo, prob)

    suite.Update(170)
    for hypo, prob in suite.Items():
        print(hypo, prob)


def test_prob_male():
    heights = np.linspace(130, 210)
    series = pd.Series(index=heights)
    suite = Height(dict(male=0.49, female=0.51))
    for height in heights:
        series[height] = suite.prob_male(height)
    thinkplot.plot(series)
    thinkplot.decorate(xlabel="Height (cm)", ylabel="Probability of being male")


@pytest.fixture(name="male_height_pmf")
def male_height_pmf_fixture():
    dist_height = dict(male=norm(178, 7.7), female=norm(163, 7.3))
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))
    return male_height_pmf


@pytest.fixture(name="female_height_pmf")
def female_height_pmf_fixture():
    dist_height = dict(male=norm(178, 7.7), female=norm(163, 7.3))
    hs = np.linspace(130, 210)
    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))
    return female_height_pmf


def test_male_height_pmf(male_height_pmf, female_height_pmf):
    """
    # If you are curious, you can derive the mathematical form of this curve from the PDF of the normal distribution.

    # ### How tall is A?
    #
    # Suppose I choose two residents of the U.S. at random.  A is taller than B.  How tall is A?
    #
    # What if I tell you that A is taller than B by more than 5 cm.  How tall is A?
    #
    # For adult male residents of the US, the mean and standard deviation of height are 178 cm and 7.7 cm.  For adult female residents the corresponding stats are 163 cm and 7.3 cm.

    # Here are distributions that represent the heights of men and women in the U.S.
    """
    thinkplot.Pdf(male_height_pmf, label="Male")
    thinkplot.Pdf(female_height_pmf, label="Female")
    thinkplot.decorate(
        xlabel="Height (cm)", ylabel="PMF", title="Adult residents of the U.S."
    )


@pytest.fixture(name="mix")
def mix_fixture(male_height_pmf, female_height_pmf):
    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)
    return mix


def test_make_mixture(male_height_pmf, female_height_pmf):
    """
    Use `thinkbayes.MakeMixture` to make a `Pmf` that represents the height of all residents of the U.S.
    """

    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)
    mix.Mean()

    # +
    # Solution

    thinkplot.Pdf(mix)

    thinkplot.decorate(
        xlabel="Height (cm)", ylabel="PMF", title="Adult residents of the U.S."
    )


class Heights(Suite, Joint):
    """
    Write a class that inherits from Suite and Joint, and provides a Likelihood function
    that computes the probability of the data under a given hypothesis.
    """

    def Likelihood(self, data, hypo):
        """

        data: who is taller, 'A' or 'B'?
        hypo: h1, h2
        """
        h1, h2 = hypo
        if data == "A":
            return 1 if h1 > h2 else 0
        else:
            return 1 if h2 > h1 else 0


def make_prior(A, B):
    """
    Write a function that initializes your `Suite` with an appropriate prior.
    We could also use MakeJoint for this

    :param A:
    :param B:
    :return:
    """
    suite = Heights()

    for h1, p1 in A.Items():
        for h2, p2 in B.Items():
            suite[h1, h2] = p1 * p2
    return suite


def test_make_prior(mix):
    suite = make_prior(mix, mix)
    suite.Total()

    thinkplot.Contour(suite)
    thinkplot.decorate(
        xlabel="B Height (cm)", ylabel="A Height (cm)", title="Posterior joint distribution"
    )


def test_update(mix):
    """
    Update your `Suite`, then plot the joint distribution and
    the marginal distribution, and compute the posterior means for `A` and `B`.
    """
    suite = make_prior(mix, mix)
    suite.Total()
    suite.Update(0)
    thinkplot.Contour(suite)
    thinkplot.decorate(
        xlabel="B Height (cm)", ylabel="A Height (cm)", title="Posterior joint distribution"
    )


def test_Marginal(male_height_pmf, female_height_pmf, mix):
    suite = make_prior(mix, mix)
    suite.Total()
    suite.Update(0)
    posterior_a = suite.Marginal(0)
    posterior_b = suite.Marginal(1)

    thinkplot.Pdf(posterior_a, label="A")
    thinkplot.Pdf(posterior_b, label="B")
    thinkplot.decorate(
        xlabel="Height (cm)", ylabel="PMF", title="Posterior marginal distributions"
    )

    posterior_a.Mean(), posterior_b.Mean()
    # -

    # ### Second tallest problem
    #
    # In a room of 10 randomly chosen U.S. residents, A is the second tallest.  How tall is A?  What is the probability that A is male?

    # +
    # Solution

    # The prior for A and B is the mixture we computed above.

    A = mix
    B = mix

    # +
    # Solution

    def faceoff(player1, player2, data):
        """Compute the posterior distributions for both players.

        player1: Pmf
        player2: Pmf
        data: margin by which player1 beats player2
        """
        joint = make_prior(player1, player2)
        joint.Update(data)
        return joint.Marginal(0), joint.Marginal(1)

    # +
    # Solution

    # We can think of the scenario as a sequence of "faceoffs"
    # where A wins 8 and loses 1

    for i in range(8):
        A, _ = faceoff(A, B, "A")

    A, B = faceoff(A, B, "B")

    # +
    # Solution

    # Here's the posterior distribution for A

    thinkplot.Pdf(A)
    A.Mean()

    # +
    # Solution

    # Now we can compute the total probability of being male,
    # conditioned on the posterior distribution of height.
    height_instance = Height()
    total = 0
    for h, p in A.Items():
        total += p * height_instance.prob_male(h)

    # +
    # Solution

    # Here's a second solution based on an "annotated" mix that keeps
    # track of M and F

    annotated_mix = Suite()
    for h, p in male_height_pmf.Items():
        annotated_mix["M", h] = p * 0.49

    for h, p in female_height_pmf.Items():
        annotated_mix["F", h] = p * 0.51

    annotated_mix.Total()

    # +
    # Solution

    # Here's an updated Heights class that can handle the
    # annotated mix

    class Heights2(Suite, Joint):
        def Likelihood(self, data, hypo):
            """

            data: who is taller, A or B
            hypo: (MF1, h1), (MF2, h2)
            """
            (_, h1), (_, h2) = hypo
            if data == "A":
                return 1 if h1 > h2 else 0
            if data == "B":
                return 1 if h2 > h1 else 0

    # +
    # Solution

    # Everything else is pretty much the same

    from thinkbayes import MakeJoint

    def faceoff(player1, player2, data):
        joint = Heights2(MakeJoint(player1, player2))
        joint.Update(data)
        return joint.Marginal(0), joint.Marginal(1)

    # +
    # Solution

    A = annotated_mix
    B = annotated_mix

    # +
    # Solution

    for i in range(8):
        A, _ = faceoff(A, B, "A")

    A, _ = faceoff(A, B, "B")

    # +
    # Solution

    # Now the posterior distribution for A contains the
    # probability of being male

    A_male = Joint(A).Marginal(0)
    print(f"A_male = {A_male}")
    # +
    # Solution

    # The posterior distribution for A also contains the
    # posterior probability of height

    A_height = Joint(A).Marginal(1)

    thinkplot.Pdf(A_height)
    A_height.Mean()
    # -
