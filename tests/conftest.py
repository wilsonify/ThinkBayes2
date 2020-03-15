import os

import pytest
from thinkbayes.utils import read_gss

TESTDIR = os.path.abspath(os.path.dirname(__file__))
DATADIR = os.path.join(TESTDIR, "data")


@pytest.fixture
def gss():
    """
    https://gssdataexplorer.norc.org/projects/52787/variables/1698/vshow
    """
    return read_gss(os.path.join(DATADIR, "gss_bayes"))
