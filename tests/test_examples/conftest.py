import os
import pandas as pd
import pytest
import thinkbayes
from thinkbayes import Hist, Pmf, Suite
from thinkbayes.utils import read_gss

CONFTESTDIR = os.path.abspath(os.path.dirname(__file__))
TESTDIR = os.path.abspath(os.path.join(CONFTESTDIR, os.pardir))
DATADIR = os.path.join(TESTDIR, "data")


@pytest.fixture
def gss():
    """
    https://gssdataexplorer.norc.org/projects/52787/variables/1698/vshow
    """
    return read_gss(os.path.join(DATADIR, "gss_bayes"))


@pytest.fixture
def six_sided_die_pmf():
    """
    a probability mass function that represents the outcome of a six-sided die.
    Initially there are 6 values with equal probability.
    :return:
    """
    pmf = thinkbayes.Pmf()
    for x in [1, 2, 3, 4, 5, 6]:
        pmf[x] = 1
    pmf.Normalize()
    return pmf


@pytest.fixture
def drp_scores_df():
    return pd.read_csv(
        os.path.join(DATADIR, "drp_scores.csv"), skiprows=21, delimiter="\t"
    )


@pytest.fixture
def flea_beetles_df():
    return pd.read_csv(os.path.join(DATADIR, "flea_beetles.csv"), delimiter="\t")


@pytest.fixture(name="d6")
def d6_fixture():
    """
        Playing dice with the universe
    One of the recurring themes of my books is the use of object-oriented programming to explore mathematical ideas.
    Many mathematical entities are hard to define because they are so abstract.
    Representing them in Python puts the focus on what operations each entity supports --
    that is, what the objects can *do* -- rather than on what they *are*.
    In this notebook, I explore the idea of a probability distribution,
    which is one of the most important ideas in statistics, but also one of the hardest to explain.

    To keep things concrete, I'll start with one of the usual examples: rolling dice.
    When you roll a standard six-sided die,
    there are six possible outcomes -- numbers 1 through 6 -- and all outcomes are equally likely.

    If you roll two dice and add up the total, there are 11 possible outcomes
    -- numbers 2 through 12 -- but they are not equally likely.
    The least likely outcomes, 2 and 12, only happen once in 36 tries;
    the most likely outcome happens 1 times in 6.

    And if you roll three dice and add them up,
    you get a different set of possible outcomes with a different set of probabilities.

    What I've just described are three random number generators,
    which are also called **random processes**.
    The output from a random process is a **random variable**,
    or more generally a set of random variables.
    And each random variable has **probability distribution**,
    which is the set of possible outcomes and the corresponding set of probabilities.

    There are many ways to represent a probability distribution.
    The most obvious is a **probability mass function**, or PMF,
    which is a function that maps from each possible outcome to its probability.
    And in Python, the most obvious way to represent a PMF is a dictionary that maps from outcomes to probabilities.

    `thinkbayes2` provides a class called `Pmf` that represents a probability mass function.
    Each `Pmf` contains a dictionary named `d` that contains the values and probabilities.
    To show how this class is used, I'll create a `Pmf` that represents a six-sided die:
    :return:
    """
    d6 = Pmf()
    for x in range(1, 7):
        d6[x] = 1
    d6.Normalize()
    return d6
