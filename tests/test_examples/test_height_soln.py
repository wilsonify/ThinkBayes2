"""
Think Bayes
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np
import pandas as pd
from scipy.stats import norm
from thinkbayes import Pmf, Suite, Joint, MakeMixture, MakeJoint
from thinkbayes import thinkplot

dist_height = dict(male=norm(178, 7.7), female=norm(163, 7.3))


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

        height = data

        return dist_height[hypo].pdf(height)


class Heights(Suite, Joint):
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


def make_prior(A, B):
    suite = Heights()

    for h1, p1 in A.Items():
        for h2, p2 in B.Items():
            suite[h1, h2] = p1 * p2
    return suite


def prob_male(height):
    suite = Height(dict(male=0.49, female=0.51))
    suite.update(height)
    return suite["male"]


def faceoff(player1, player2, data):
    """
    Compute the posterior distributions for both players.
    player1: Pmf
    player2: Pmf
    data: margin by which player1 beats player2
    """
    joint = make_prior(player1, player2)
    joint.update(data)
    return joint.Marginal(0), joint.Marginal(1)


def test_one():
    suite = Height(dict(male=0.49, female=0.51))
    for hypo, prob in suite.Items():
        print(hypo, prob)

    suite.update(170)
    for hypo, prob in suite.Items():
        print(hypo, prob)

    heights = np.linspace(130, 210)
    series = pd.Series(index=heights, dtype=np.float64)

    for height in heights:
        series[height] = prob_male(height)

    thinkplot.plot(series)
    thinkplot.decorate(xlabel="Height (cm)", ylabel="Probability of being male")


def test_two():
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))

    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))

    thinkplot.plot_pdf_line(male_height_pmf, label="Male")
    thinkplot.plot_pdf_line(female_height_pmf, label="Female")

    thinkplot.decorate(
        xlabel="Height (cm)", ylabel="PMF", title="Adult residents of the U.S."
    )


def test_three():
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))

    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))

    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)
    mix.Mean()

    thinkplot.plot_pdf_line(mix)
    thinkplot.decorate(
        xlabel="Height (cm)", ylabel="PMF", title="Adult residents of the U.S."
    )


def test_four():
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))

    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))

    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)
    mix.Mean()

    suite = make_prior(mix, mix)
    suite.Total()

    thinkplot.contour_plot(suite)
    thinkplot.decorate(
        xlabel="B Height (cm)",
        ylabel="A Height (cm)",
        title="Posterior joint distribution",
    )


def test_five():
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))

    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))

    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)
    mix.Mean()

    suite = make_prior(mix, mix)
    suite.Total()
    suite.update(0)

    thinkplot.contour_plot(suite)
    thinkplot.decorate(
        xlabel="B Height (cm)",
        ylabel="A Height (cm)",
        title="Posterior joint distribution",
    )


def test_six():
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))

    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))

    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)
    mix.Mean()

    suite = make_prior(mix, mix)
    suite.Total()
    suite.update(0)

    posterior_a = suite.Marginal(0)
    posterior_b = suite.Marginal(1)

    thinkplot.plot_pdf_line(posterior_a, label="A")
    thinkplot.plot_pdf_line(posterior_b, label="B")
    thinkplot.decorate(
        xlabel="Height (cm)", ylabel="PMF", title="Posterior marginal distributions"
    )

    posterior_a.Mean(), posterior_b.Mean()


def test_seven():
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))

    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))

    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)

    A = mix
    B = mix

    for i in range(8):
        A, _ = faceoff(A, B, "A")

    A, B = faceoff(A, B, "B")

    thinkplot.plot_pdf_line(A)
    A.Mean()


def test_eight():
    hs = np.linspace(130, 210)
    ps = dist_height["male"].pdf(hs)
    male_height_pmf = Pmf(dict(zip(hs, ps)))

    ps = dist_height["female"].pdf(hs)
    female_height_pmf = Pmf(dict(zip(hs, ps)))

    metapmf = Pmf({male_height_pmf: 0.49, female_height_pmf: 0.51})
    mix = MakeMixture(metapmf)

    A = mix
    B = mix

    total = 0
    for h, p in A.Items():
        total += p * prob_male(h)
    logging.info("%r", f"total = {total}")


    annotated_mix = Suite()
    for h, p in male_height_pmf.Items():
        annotated_mix["M", h] = p * 0.49

    for h, p in female_height_pmf.Items():
        annotated_mix["F", h] = p * 0.51

    annotated_mix.Total()

    def faceoff(player1, player2, data):
        joint = Heights2(MakeJoint(player1, player2))
        joint.update(data)
        return joint.Marginal(0), joint.Marginal(1)

    A = annotated_mix
    B = annotated_mix

    for i in range(8):
        A, _ = faceoff(A, B, "A")

    A, _ = faceoff(A, B, "B")

    A_male = Joint(A).Marginal(0)

    A_height = Joint(A).Marginal(1)
    thinkplot.plot_pdf_line(A_height)
    A_height.Mean()
