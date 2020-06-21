"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf


def test_smoking_problem():
    # ## Smoking problem
    #
    # According to the CDC, "Compared to nonsmokers, men who smoke are about 23 times more likely to develop lung cancer and women who smoke are about 13 times more likely.''
    #
    # Also, among adults in the U.S. in 2014:
    #
    # > Nearly 19 of every 100 adult men (18.8%)
    # > Nearly 15 of every 100 adult women (14.8%)
    #
    # **Exercise:** If you learn that a woman has been diagnosed with lung cancer, and you know nothing else about her, what is the probability that she is a smoker?

    # Solution

    # In this case, we can't compute the likelihoods individually;
    # we only know the ratio of one to the other.  But that's enough.

    # Two ways to proceed: we could include a variable in the computation,
    # and we would see it drop out.

    # Or we can use "unnormalized likelihoods", for want of a better term.

    # Here's my solution.

    pmf = Pmf(dict(smoker=15, nonsmoker=85))
    pmf["smoker"] *= 13
    pmf["nonsmoker"] *= 1
    pmf.normalize()
    pmf.print()
