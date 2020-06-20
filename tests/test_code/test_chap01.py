"""
This notebook presents example code and exercise solutions for Think Bayes.
"""

import numpy as np


def values(series):
    return series.value_counts().sort_index()


def prob(A):
    """Probability of A"""
    return A.mean()


def count(A):
    """Number of instances of A"""
    return A.sum()


def conditional(A, B):
    """Conditional probability of A given B"""
    return prob(A[B])


def conjunction(A, B):
    """Probability of both A and B"""
    return prob(A) * conditional(A=B, B=A)


def bayes_theorem(A, B):
    """Conditional probability of A given B, using Bayes's theorem"""
    return prob(A) * conditional(A=B, B=A) / prob(B)


def test_total(gss):
    gss.feminist.replace([0, 8, 9], np.nan, inplace=True)
    gss.polviews.replace([0, 8, 9], np.nan, inplace=True)
    gss.partyid.replace([8, 9], np.nan, inplace=True)
    gss.indus10.replace([0], np.nan, inplace=True)
    gss.occ10.replace([0], np.nan, inplace=True)

    values(gss.feminist)
    values(gss.polviews)
    values(gss.partyid)
    values(gss.race)
    values(gss.sex)
    values(gss.indus10).head()
    np.mean(gss.indus10 == 6870)

    (gss.indus10 == 6870).mean()

    subset = gss.dropna(subset=["sex", "polviews", "partyid", "indus10"])
    assert subset.shape == (50287, 19)

    female = gss.sex == 2
    values(female)

    liberal = gss.polviews <= 2
    values(liberal)

    democrat = gss.partyid <= 1
    values(democrat)

    banker = gss.indus10 == 6870
    values(banker)

    total = 0
    for x in banker:
        if x is True:
            total += 1

    print(total / len(banker))

    prob(female)

    prob(liberal)

    prob(democrat)

    prob(banker)

    prob(democrat & liberal)

    count(banker[female])

    prob(banker[female])

    prob(female & banker)

    banker_given_female = prob(banker & female) / prob(female)
    print(banker_given_female)

    conditional(banker, female)

    conditional(liberal, democrat)

    conditional(democrat, liberal)

    conditional(democrat, female)

    prob(liberal & democrat)

    conjunction(liberal, democrat)

    liberal_and_democrat = prob(liberal) * prob(democrat)
    print(liberal_and_democrat)

    conjunction(democrat, liberal)
    female_given_banker = conditional(female, banker)
    banker_given_female = prob(banker) * female_given_banker / prob(female)
    print(banker_given_female)

    bayes_theorem(democrat, liberal)

    conditional(banker, female)

    conditional(banker, female & liberal)

    conditional(banker & democrat, female & liberal)
