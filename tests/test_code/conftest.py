import os

import pandas as pd
import pytest
import thinkbayes
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
    pmf.normalize()
    return pmf


@pytest.fixture
def drp_scores_df():
    return pd.read_csv(
        os.path.join(DATADIR, "drp_scores.csv"), skiprows=21, delimiter="\t"
    )


@pytest.fixture
def flea_beetles_df():
    return pd.read_csv(os.path.join(DATADIR, "flea_beetles.csv"), delimiter="\t")
