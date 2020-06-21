"""This file contains code used in "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging
import math

import thinkbayes
from thinkbayes import thinkplot


class Sat(thinkbayes.Suite, thinkbayes.Joint):
    """Represents the distribution of p_correct for a test-taker."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of data under hypo.

        data: boolean, whether the answer is correct
        hypo: pair of (efficacy, difficulty)
        """
        # TODO: fill this in
        like = 1
        return like


def ProbCorrect(efficacy, difficulty, a=1):
    """Returns the probability that a person gets a question right.

    efficacy: personal ability to answer questions
    difficulty: how hard the question is
    a: parameter that controls the shape of the curve

    Returns: float prob
    """
    return 1 / (1 + math.exp(-a * (efficacy - difficulty)))


def Update(p, q, correct):
    """Updates p and q according to correct.

    p: prior distribution of efficacy for the test-taker
    q: prior distribution of difficulty for the question

    returns: pair of new Pmfs
    """
    logging.debug("%r", f"correct={correct}")
    # TODO: fill this in
    # HINT: form a joint distribution, update it, then extract marginals
    return p, q


def main():
    p1 = thinkbayes.MakeNormalPmf(0, 1, 3, n=101)
    p1.label = "p1"
    p2 = p1.Copy(label="p2")

    q1 = thinkbayes.MakeNormalPmf(-1, 1, 3, n=101)
    q1.label = "q1"
    q2 = q1.Copy(label="q2")

    p1, q1 = Update(p1, q1, True)
    p1, q2 = Update(p1, q2, True)
    p2, q1 = Update(p2, q1, True)
    p2, q2 = Update(p2, q2, False)

    thinkplot.pre_plot(num=4, rows=2)
    thinkplot.plot_pmfs([p1, p2])
    thinkplot.config_plot(legend=True)

    thinkplot.sub_plot(2)
    thinkplot.plot_pmfs([q1, q2])
    thinkplot.show_plot()

    print("Prob p1 > p2", p1 > p2)
    print("Prob q1 > q2", q1 > q2)


if __name__ == "__main__":
    main()
