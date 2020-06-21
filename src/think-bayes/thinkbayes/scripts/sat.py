"""This file contains code used in "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import csv
import logging
import math
import sys

import numpy
import thinkbayes
from thinkbayes import thinkplot


def read_scale(filename="sat_scale.csv", col=2):
    """Reads a CSV file of SAT scales (maps from raw score to standard score).

    Args:
      filename: string filename
      col: which column to start with (0=Reading, 2=Math, 4=Writing)

    Returns: thinkbayes.Interpolator object
    """

    def parse_range(s):
        """Parse a range of values in the form 123-456

        s: string
        """
        s_split = [int(x) for x in s.split("-")]
        return 1.0 * sum(s_split) / len(s_split)

    fp = open(filename)
    reader = csv.reader(fp)
    raws = []
    scores = []

    for t in reader:
        try:
            raw = int(t[col])
            raws.append(raw)
            score = parse_range(t[col + 1])
            scores.append(score)
        except ValueError:
            pass

    raws.sort()
    scores.sort()
    return thinkbayes.Interpolator(raws, scores)


def read_ranks(filename="sat_ranks.csv"):
    """Reads a CSV file of SAT scores.

    Args:
      filename: string filename

    Returns:
      list of (score, freq) pairs
    """
    fp = open(filename)
    reader = csv.reader(fp)
    res = []

    for t in reader:
        try:
            score = int(t[0])
            freq = int(t[1])
            res.append((score, freq))
        except ValueError:
            pass

    return res


def divide_values(pmf, denom):
    """Divides the values in a Pmf by denom.

    Returns a new Pmf.
    """
    new = thinkbayes.Pmf()
    denom = float(denom)
    for val, prob in pmf.items():
        x = val / denom
        new.set(x, prob)
    return new


class Exam(object):
    """Encapsulates information about an exam.

    Contains the distribution of scaled scores and an
    Interpolator that maps between scaled and raw scores.
    """

    def __init__(self):
        self.scale = read_scale()

        scores = read_ranks()
        score_pmf = thinkbayes.Pmf(dict(scores))

        self.raw = self.reverse_scale(score_pmf)
        self.max_score = max(self.raw.values())
        self.prior = divide_values(self.raw, denom=self.max_score)

        center = -0.05
        width = 1.8
        self.difficulties = make_difficulties(center, width, self.max_score)

    def compare_scores(self, a_score, b_score, constructor):
        """Computes posteriors for two test scores and the likelihood ratio.

        a_score, b_score: scales SAT scores
        constructor: function that instantiates an Sat or Sat2 object
        """
        a_sat = constructor(self, a_score)
        b_sat = constructor(self, b_score)

        a_sat.plot_posteriors(b_sat)

        if constructor is Sat:
            plot_joint_dist(a_sat, b_sat)

        top = TopLevel("AB")
        top.update((a_sat, b_sat))
        top.print()

        ratio = top.prob("A") / top.prob("B")

        print("Likelihood ratio", ratio)

        posterior = ratio / (ratio + 1)
        print("Posterior", posterior)

        if constructor is Sat2:
            compare_posterior_predictive(a_sat, b_sat)

    def make_raw_score_dist(self, efficacies):
        """Makes the distribution of raw scores for given difficulty.

        efficacies: Pmf of efficacy
        """
        pmfs = thinkbayes.Pmf()
        for efficacy, prob in efficacies.items():
            scores = self.pmf_correct(efficacy)
            pmfs.set(scores, prob)

        mix = thinkbayes.make_mixture(pmfs)
        return mix

    def calibrate_difficulty(self):
        """Make a plot showing the model distribution of raw scores."""
        thinkplot.clear_figure()
        thinkplot.pre_plot(num=2)

        cdf = thinkbayes.Cdf(self.raw, label="data")
        thinkplot.plot_cdf_line(cdf)

        efficacies = thinkbayes.make_normal_pmf(0, 1.5, 3)
        pmf = self.make_raw_score_dist(efficacies)
        cdf = thinkbayes.Cdf(pmf, label="model")
        thinkplot.plot_cdf_line(cdf)

        thinkplot.save_plot(
            root="sat_calibrate",
            xlabel="raw score",
            ylabel="CDF",
            formats=["pdf", "eps"],
        )

    def pmf_correct(self, efficacy):
        """Returns the PMF of number of correct responses.

        efficacy: float
        """
        pmf = pmf_correct(efficacy, self.difficulties)
        return pmf

    def lookup(self, raw):
        """Looks up a raw score and returns a scaled score."""
        return self.scale.lookup(raw)

    def reverse(self, score):
        """Looks up a scaled score and returns a raw score.

        Since we ignore the penalty, negative scores round up to zero.
        """
        raw = self.scale.reverse(score)
        return raw if raw > 0 else 0

    def reverse_scale(self, pmf):
        """Applies the reverse scale to the values of a PMF.

        Args:
            pmf: Pmf object

        Returns:
            new Pmf
        """
        new = thinkbayes.Pmf()
        for val, prob in pmf.items():
            raw = self.reverse(val)
            new.incr(raw, prob)
        return new


class Sat(thinkbayes.Suite):
    """Represents the distribution of p_correct for a test-taker."""

    def __init__(self, exam, score):
        self.exam = exam
        self.score = score

        # start with the prior distribution
        thinkbayes.Suite.__init__(self, exam.prior)

        # update based on an exam score
        self.update(score)

    def likelihood(self, data, hypo):
        """Computes the likelihood of a test score, given efficacy."""
        p_correct = hypo
        score = data

        k = self.exam.reverse(score)
        n = self.exam.max_score
        like = thinkbayes.eval_binomial_pmf(k, n, p_correct)
        return like

    def plot_posteriors(self, other):
        """Plots posterior distributions of efficacy.

        self, other: Sat objects.
        """
        thinkplot.clear_figure()
        thinkplot.pre_plot(num=2)

        cdf1 = thinkbayes.Cdf(self, label=f"posterior {self.score}")
        cdf2 = thinkbayes.Cdf(other, label=f"posterior {other.score}")

        thinkplot.plot_cdfs([cdf1, cdf2])
        thinkplot.save_plot(
            xlabel="p_correct",
            ylabel="CDF",
            axis=[0.7, 1.0, 0.0, 1.0],
            root="sat_posteriors_p_corr",
            formats=["pdf", "eps"],
        )


class Sat2(thinkbayes.Suite):
    """Represents the distribution of efficacy for a test-taker."""

    def __init__(self, exam, score):
        self.exam = exam
        self.score = score

        # start with the Normal prior
        efficacies = thinkbayes.make_normal_pmf(0, 1.5, 3)
        thinkbayes.Suite.__init__(self, efficacies)

        # update based on an exam score
        self.update(score)

    def likelihood(self, data, hypo):
        """Computes the likelihood of a test score, given efficacy."""
        efficacy = hypo
        score = data
        raw = self.exam.reverse(score)

        pmf = self.exam.pmf_correct(efficacy)
        like = pmf.prob(raw)
        return like

    def make_predictive_dist(self):
        """Returns the distribution of raw scores expected on a re-test."""
        raw_pmf = self.exam.make_raw_score_dist(self)
        return raw_pmf

    def plot_posteriors(self, other):
        """Plots posterior distributions of efficacy.

        self, other: Sat objects.
        """
        thinkplot.clear_figure()
        thinkplot.pre_plot(num=2)

        cdf1 = thinkbayes.Cdf(self, label=f"posterior {self.score}")
        cdf2 = thinkbayes.Cdf(other, label=f"posterior {other.score}")

        thinkplot.plot_cdfs([cdf1, cdf2])
        thinkplot.save_plot(
            xlabel="efficacy",
            ylabel="CDF",
            axis=[0, 4.6, 0.0, 1.0],
            root="sat_posteriors_eff",
            formats=["pdf", "eps"],
        )


def plot_joint_dist(pmf1, pmf2, thresh=0.8):
    """Plot the joint distribution of p_correct.

    pmf1, pmf2: posterior distributions
    thresh: lower bound of the range to be plotted
    """

    def clean(probability_mass_function):
        """Removes values below thresh."""
        vals = [val for val in probability_mass_function.values() if val < thresh]
        [probability_mass_function.remove(val) for val in vals]

    clean(pmf1)
    clean(pmf2)
    pmf = thinkbayes.make_joint(pmf1, pmf2)

    thinkplot.underride_figure(figsize=(6, 6))
    thinkplot.contour_plot(pmf, contour_bool=False, pcolor_bool=True)

    thinkplot.plot_line([thresh, 1.0], [thresh, 1.0], color="gray", alpha=0.2, linewidth=4)

    thinkplot.save_plot(
        root="sat_joint",
        xlabel="p_correct Alice",
        ylabel="p_correct Bob",
        axis=[thresh, 1.0, thresh, 1.0],
        formats=["pdf", "eps"],
    )


def compare_posterior_predictive(a_sat, b_sat):
    """Compares the predictive distributions of raw scores.

    a_sat: posterior distribution
    b_sat:
    """
    a_pred = a_sat.make_predictive_dist()
    b_pred = b_sat.make_predictive_dist()

    # thinkplot.Clf()
    # thinkplot.Pmfs([a_pred, b_pred])
    # thinkplot.Show()

    a_like = thinkbayes.pmf_prob_greater(a_pred, b_pred)
    b_like = thinkbayes.pmf_prob_less(a_pred, b_pred)
    c_like = thinkbayes.pmf_prob_equal(a_pred, b_pred)

    print("Posterior predictive")
    print("A", a_like)
    print("B", b_like)
    print("C", c_like)


def plot_prior_dist(pmf):
    """Plot the prior distribution of p_correct.

    pmf: prior
    """
    thinkplot.clear_figure()
    thinkplot.pre_plot(num=1)

    cdf1 = thinkbayes.Cdf(pmf, label="prior")

    thinkplot.plot_cdf_line(cdf1)
    thinkplot.save_plot(
        root="sat_prior", xlabel="p_correct", ylabel="CDF", formats=["pdf", "eps"]
    )


class TopLevel(thinkbayes.Suite):
    """Evaluates the top-level hypotheses about Alice and Bob.

    Uses the bottom-level posterior distribution about p_correct
    (or efficacy).
    """

    def update(self, data):
        a_sat, b_sat = data

        a_like = thinkbayes.pmf_prob_greater(a_sat, b_sat)
        b_like = thinkbayes.pmf_prob_less(a_sat, b_sat)
        c_like = thinkbayes.pmf_prob_equal(a_sat, b_sat)

        a_like += c_like / 2
        b_like += c_like / 2

        self.mult("A", a_like)
        self.mult("B", b_like)

        self.normalize()


def prob_correct(efficacy, difficulty, a=1):
    """Returns the probability that a person gets a question right.

    efficacy: personal ability to answer questions
    difficulty: how hard the question is

    Returns: float prob
    """
    return 1 / (1 + math.exp(-a * (efficacy - difficulty)))


def binary_pmf(p):
    """Makes a Pmf with values 1 and 0.
    
    p: probability given to 1
    
    Returns: Pmf object
    """
    pmf = thinkbayes.Pmf()
    pmf.set(1, p)
    pmf.set(0, 1 - p)
    return pmf


def pmf_correct(efficacy, difficulties):
    """Computes the distribution of correct responses.

    efficacy: personal ability to answer questions
    difficulties: list of difficulties, one for each question

    Returns: new Pmf object
    """
    pmf0 = thinkbayes.Pmf([0])

    ps = [prob_correct(efficacy, difficulty) for difficulty in difficulties]
    pmfs = [binary_pmf(p) for p in ps]
    dist = sum(pmfs, pmf0)
    return dist


def make_difficulties(center, width, n):
    """Makes a list of n difficulties with a given center and width.

    Returns: list of n floats between center-width and center+width
    """
    low, high = center - width, center + width
    return numpy.linspace(low, high, n)


def prob_correct_table():
    """Makes a table of p_correct for a range of efficacy and difficulty."""
    efficacies = [3, 1.5, 0, -1.5, -3]
    difficulties = [-1.85, -0.05, 1.75]

    for eff in efficacies:
        print("%0.2f & " % eff, end=" ")
        for diff in difficulties:
            p = prob_correct(eff, diff)
            print("%0.2f & " % p, end=" ")
        print(r"\\")


def main(script):
    logging.debug("%r", f"script={script}")
    prob_correct_table()

    exam = Exam()

    plot_prior_dist(exam.prior)
    exam.calibrate_difficulty()

    exam.compare_scores(780, 740, constructor=Sat)

    exam.compare_scores(780, 740, constructor=Sat2)


if __name__ == "__main__":
    main(*sys.argv)
