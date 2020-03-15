import os

import pytest
import thinkbayes
from thinkbayes import Hist, Pmf, Suite
from thinkbayes.utils import read_gss

TESTDIR = os.path.abspath(os.path.dirname(__file__))
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
