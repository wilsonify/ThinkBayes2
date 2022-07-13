"""
This is based on code and exercises from Think Bayes: Chapter 7.
"""

from scipy.stats import poisson

from thinkbayes import MakeExponentialPmf
from thinkbayes import MakeMixture
from thinkbayes import MakeNormalPmf
from thinkbayes import Pmf, Suite


class Hockey(Suite):
    """Represents hypotheses about the scoring rate for a team."""

    def __init__(self, label=None):
        """Initializes the Hockey object.

        label: string
        """
        mu = 2.8
        sigma = 0.3

        pmf = MakeNormalPmf(mu, sigma, num_sigmas=4, n=101)
        Suite.__init__(self, pmf, label=label)

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        Evaluates the Poisson PMF for lambda and k.

        hypo: goal scoring rate in goals per game
        data: goals scored in one game
        """
        pmf = MakePoissonPmf(lam=hypo, qs=data)
        return pmf.MaximumLikelihood


def MakeGoalPmf(suite, high=10):
    """Makes the distribution of goals scored, given distribution of lam.

    suite: distribution of goal-scoring rate
    high: upper bound

    returns: Pmf of goals per game
    """
    metapmf = Pmf()

    for lam, prob in suite.Items():
        pmf = MakePoissonPmf(lam, high)
        metapmf.Set(pmf, prob)

    mix = MakeMixture(metapmf, label=suite.label)
    return mix


def MakeGoalTimePmf(suite):
    """Makes the distribution of time til first goal.

    suite: distribution of goal-scoring rate

    returns: Pmf of goals per game
    """
    metapmf = Pmf()

    for lam, prob in suite.Items():
        pmf = MakeExponentialPmf(lam, high=2.5, n=1001)
        metapmf.Set(pmf, prob)

    mix = MakeMixture(metapmf, label=suite.label)
    return mix


def MakePoissonPmf(lam, qs):
    """Make a PMF of a Poisson distribution.

    lam: event rate
    qs: sequence of values for `k`

    returns: Pmf
    """
    ps = poisson(lam).pmf(qs)
    pmf = Pmf(ps, qs)
    pmf.Normalize()
    return pmf
