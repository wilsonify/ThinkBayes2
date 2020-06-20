"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging
import os

import numpy as np
import pandas as pd
import pytest
from scipy.stats import poisson
from thinkbayes import Suite, Joint
from thinkbayes import thinkplot

CURDIR = os.path.dirname(__file__)
PARDIR = os.path.join(CURDIR, os.pardir)
TESTDIR = os.path.abspath(PARDIR)
DATADIR = os.path.join(TESTDIR, "data")


@pytest.fixture(name="n")
def n_fixture(n):
    n = 60
    return n


@pytest.fixture(name="t1")
def t1_fixture(t1):
    t1 = 30
    return t1


@pytest.fixture(name="t2")
def t2_fixture(n, t1):
    t2 = n - t1
    return t2


@pytest.fixture(name="lam1")
def lam1_fixture():
    lam1 = 4
    return lam1


@pytest.fixture(name="lam2")
def lam2_fixture():
    lam2 = 2
    return lam2


@pytest.fixture(name="before")
def before_fixture(lam1, t1):
    before = poisson(lam1).rvs(t1)
    return before


@pytest.fixture(name="after")
def after_fixture(lam2, t2):
    after = poisson(lam2).rvs(t2)
    return after


@pytest.fixture(name="data")
def data_fixture(before, after):
    data = np.concatenate([before, after])
    return data


class Change(Suite, Joint):
    def Likelihood(self, data, hypo):
        """
        
        data: array of counts
        hypo: t, lam1, lam2
        """
        # FILL THIS IN
        return 1


@pytest.fixture(name="crime_data_df")
def crime_data_fixture():
    logging.debug("%r", f"DATADIR = {DATADIR}")
    crime_data_file_path = os.path.join(
        DATADIR, "BPD_Part_1_Victim_Based_Crime_Data.csv"
    )
    crime_data_df = pd.read_csv(crime_data_file_path, parse_dates=["CrimeDate"])
    return crime_data_df


def test_shootings(crime_data_df):
    """

    MCMC

    To implement this model in PyMC,
    see Chapter 1 of [Bayesian Methods for Hackers](http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Ch1_Introduction_PyMC2.ipynb)
    and this example from [Computational Statistics in Python](http://people.duke.edu/~ccc14/sta-663-2016/16C_PyMC3.html#Changepoint-detection)

    Some real data,
    based on
    [this analysis from the Baltimore Sun](http://www.baltimoresun.com/news/maryland/crime/bs-md-ci-violence-stats-20181018-story.html)
    https://raw.githubusercontent.com/baltimore-sun-data/2018-shootings-analysis/master/BPD_Part_1_Victim_Based_Crime_Data.csv


    :param crime_data_df:
    :return:
    """

    is_description = crime_data_df.Description.isin(["HOMICIDE", "SHOOTING"])
    is_weapon = crime_data_df.Weapon == "FIREARM"
    shootings = crime_data_df[is_description & is_weapon]
    shootings.shape

    grouped = shootings.groupby("CrimeDate")
    counts = grouped["Total Incidents"].sum()

    index = pd.date_range(counts.index[0], counts.index[-1])

    counts = counts.reindex(index, fill_value=0)
    counts.head()

    counts.plot.line()
    thinkplot.decorate(xlabel="Date", ylabel="Number of shootings")

    # plt.savefig("Number of shootings.png")
