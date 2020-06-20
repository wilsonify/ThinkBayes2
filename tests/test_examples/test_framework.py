"""
The ThinkBayes framework
Introduction
thinkbayes is a Python module that provides a framework for Bayesian statistics.

The most important class is `Suite`, which represents
a set of hypotheses that are mutually exclusive and collectively exhaustive;
in other words, exactly one of the hypotheses is true.  
`Suite` provides a method, `Update`, that performs a Bayesian update.

`Suite` is based on `Pmf`, which represents a probability mass function.
A Pmf is a map from possible values to their probabilities.  
`Pmf` is implemented using a Python dictionary, so the values can be any hashable type.  
The probabilities are normally floating-point.

`Suite` provides `Update`, which performs a Bayesian update.
It calls `Likelihood`, which is provided by a child class, `Coin` in this example.

It also uses `Normalize`, which is provided by `Pmf`.
To use this framework, you normally define a new class that inherits from `Suite`, then provide `Likelihood`.

`thinkplot` is a module that comes with thinkbayes.
It provides wrappers for some of the functions in Matplotlib,
and includes functions like `Pdf` that known how to plot `Suites` and other objects from thinkbayes.
"""

import thinkbayes
from thinkbayes import thinkplot


class Coin(thinkbayes.Suite):
    """
    Suppose you are given a strange new coin and you notice that it is unbalanced.
    When spun on edge, it seems to land with the "heads" side up more often than the "tails" side.
    You might want to estimate the probability of landing "heads" up, which I'll call $x$.
    """

    def Likelihood(self, data, hypo):
        x = hypo / 100
        if data == "H":
            return x
        else:
            return 1 - x


def test_coin():
    """
    The `Likelihood` function computes the likelihood of the data under a given hypothesis.
    In this example, `data` is a string, either `"H"` or `"T"`;
    `hypo` is the hypothetical value of $x$ in the range 0 to 100.
    The next step is to instantiate a Coin suite that represents the prior distribution of $x$.
    To start simple, I'll use a uniform distribution on the integers from 0 to 100:
    As expected, the prior is uniform.
    """
    suite = Coin(range(0, 101))
    thinkplot.Pdf(suite)


def test_update():
    """
    The next step is to update the prior with data.
    Since Update modifies the Suite, I'll make a copy before updating.

    The return value from Update is the normalizing constant,
    which is the average likelihood of the data across all hypotheses.

    After the update, large values of $x$ are more likely than small ones,
    and the hypothesis that $x=0$ has been eliminated.
    """
    suite = Coin(range(0, 101))
    posterior = suite.Copy()
    posterior.Update("H")
    thinkplot.Pdf(posterior)


def test_additional():
    """
    With additional data we can do a sequence of updates.
    The result shows the cumulative effect of all updates.
    This distribution shows what what we should believe about $x$ after seeing this data
    (based on the assumption of a uniform prior).
    """
    suite = Coin(range(0, 101))
    posterior = suite.Copy()
    results = "HTHHTHHHTTHHHTH"
    for data in results:
        posterior.Update(data)
    thinkplot.Pdf(posterior)
