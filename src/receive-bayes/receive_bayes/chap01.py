"""
This notebook presents example code and exercise solutions for Think Bayes.
"""
import logging
import os.path

import numpy as np
import pandas as pd

from thinkbayes.c01_probability import (
    values,
    prob,
    count,
    conditional,
    conjunction,
    bayes_theorem
)


def prob_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    result = prob(a)
    self.publish(result)


def conjunction_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    b = pd.Series(body["b"]).astype(bool)
    logging.debug(f"a.shape={a.shape}")
    logging.debug(f"b.shape={b.shape}")
    result = conjunction(a, b)
    logging.debug(f"result={result}")
    self.publish(result)


def conditional_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    b = pd.Series(body["b"]).astype(bool)
    result = conditional(a, b)
    self.publish(result)


def bayes_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    b = pd.Series(body["b"]).astype(bool)
    result = bayes_theorem(a, b)
    self.publish(result)


def total_strategy(self, body):
    DATADIR = os.path.abspath(__file__)
    gss = pd.read_csv(f'{DATADIR}/gss_bayes.csv', index_col=0)
    """
    • caseid : Respondent id (which is the index of the table).
    • year : Year when the respondent was surveyed.
    • age : Respondent’s age when surveyed.
    • sex : Male or female.
    • polviews : Political views on a range from liberal to conservative.
    The values of polviews are on a seven-point scale:
        1: Extremely liberal
        2: Liberal
        3: Slightly liberal
        4: Moderate
        5: Slightly conservative
        6: Conservative
        7: Extremely conservative
    • partyid : Political party affiliation: Democratic, Republican, or independent.
    The values of partyid are encoded like this:
        0: Strong democrat
        1: Not strong democrat
        2: Independent, near democrat
        3: Independent
        4: Independent, near republican
        5: Not strong republican
        6: Strong republican
        7: Other party
    • indus10 : Code for the industry the respondent works in.    
    """

    # gss.feminist.replace([0, 8, 9], np.nan, inplace=True)
    gss.polviews.replace([0, 8, 9], np.nan, inplace=True)
    gss.partyid.replace([8, 9], np.nan, inplace=True)
    gss.indus10.replace([0], np.nan, inplace=True)
    # gss.occ10.replace([0], np.nan, inplace=True)

    # values(gss.feminist)
    values(gss.polviews)
    values(gss.partyid)
    # values(gss.race)
    values(gss.sex)
    values(gss.indus10).head()
    np.mean(gss.indus10 == 6870)

    (gss.indus10 == 6870).mean()

    subset = gss.dropna(subset=["sex", "polviews", "partyid", "indus10"])
    assert subset.shape == (49290, 6)

    female = gss.sex == 2

    liberal = gss.polviews <= 2

    democrat = gss.partyid <= 1

    banker = gss.indus10 == 6870

    total = banker.astype(float).sum()
    count(banker[female])

    prob(female)
    prob(liberal)
    prob(democrat)
    prob(banker)

    prob(banker[female])
    prob(democrat & liberal)
    prob(female & banker)
    prob(liberal & democrat)
    conditional(banker, female)
    conditional(liberal, democrat)
    conditional(democrat, liberal)
    conditional(democrat, female)

    conjunction(liberal, democrat)
    conjunction(democrat, liberal)
    liberal_and_democrat = prob(liberal) * prob(democrat)

    female_given_banker = conditional(female, banker)
    banker_given_female1 = prob(banker & female) / prob(female)
    banker_given_female2 = prob(banker) * female_given_banker / prob(female)

    conditional(banker, female)
    conditional(banker, female & liberal)
    conditional(banker & democrat, female & liberal)

    values(female)
    values(liberal)
    values(democrat)
    values(banker)
    print(total / len(banker))
    print(banker_given_female1)
    print(banker_given_female2)
    print(liberal_and_democrat)
    assert banker_given_female1 == banker_given_female2
    bayes_theorem(democrat, liberal)
