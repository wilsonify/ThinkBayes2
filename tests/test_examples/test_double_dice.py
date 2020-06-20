"""
The double dice problem
This notebook demonstrates a way of doing simple Bayesian updates
using the table method, with a Pandas DataFrame as the table.
Copyright 2018 Allen Downey
MIT License: https://opensource.org/licenses/MIT
"""
from fractions import Fraction

import numpy as np
import pandas as pd
import pytest


class BayesTable(pd.DataFrame):
    """
    The BayesTable class
    Here's the class that represents a Bayesian table.
    """

    def __init__(self, hypo, prior=1, **options):
        columns = ["hypo", "prior", "likelihood", "unnorm", "posterior"]
        super().__init__(columns=columns, **options)
        self.hypo = hypo
        self.prior = prior
        self.unnorm = self["unnorm"]
        self.posterior = self["posterior"]

    def mult(self):
        self.unnorm = self.prior * self.likelihood

    def norm(self):
        nc = np.sum(self.unnorm)
        self.posterior = self.unnorm / nc
        return nc

    def bayesian_update(self):
        self.mult()
        return self.norm()

    def reset(self):
        return BayesTable(self.hypo, self.posterior)


@pytest.fixture(name="table")
def table_fixture():
    """
    The double dice problem
    Suppose I have a box that contains one each of 4-sided, 6-sided, 8-sided, and 12-sided dice.
    I choose a die at random, and roll it twice without letting you see the die or the outcome.
    I report that I got the same outcome on both rolls.



    Here's a `BayesTable` that represents the four hypothetical dice.
    Since we didn't specify prior probabilities, the default value is equal priors for all hypotheses.
    They don't have to be normalized, because we have to normalize the posteriors anyway.
    Now we can specify the likelihoods: if a die has `n` sides, the chance of getting the same outcome twice is `1/n`.
    So the likelihoods are:

    """
    hypo = [Fraction(sides) for sides in [4, 6, 8, 12]]
    table = BayesTable(hypo)
    table.likelihood = 1 / table.hypo
    return table


def test_update(table):
    """
    1) What is the posterior probability that I rolled each of the dice?
    Now we can use `update` to compute the posterior probabilities:
    The 4-sided die is most likely because you are more likely to get doubles
    on a 4-sided die than on a 6-, 8-, or 12- sided die.
    """

    table.bayesian_update()
    table.posterior.astype(float)


def test_add_up_conditional(table):
    """
    2) If I roll the same die again, what is the probability that I get the same outcome a third time?

    The second part of the problem asks for the (posterior predictive) probability
    of getting the same outcome a third time, if we roll the same die again.

    If the die has `n` sides, the probability of getting the same value again is `1/n`, which should look familiar.
    To get the total probability of getting the same outcome, we have to add up the conditional probabilities:
    $P(n | data) * P(same outcome | n)$
    The first term is the posterior probability; the second term is `1/n`.
    """
    table.bayesian_update()
    total = 0
    for _, row in table.iterrows():
        total += row.posterior / row.hypo
    assert total == Fraction(13, 72)


def test_norm(table):
    """

    This calculation is similar to the first step of the update, so we can also compute it by
    1) Creating a new table with the posteriors from `table`.
    2) Adding the likelihood of getting the same outcome a third time.
    3) Computing the normalizing constant.

    This result is the same as the posterior after seeing the same outcome three times.
    This example demonstrates a general truth:
    to compute the predictive probability of an event, you can pretend you saw the event,
    do a Bayesian update, and record the normalizing constant.
    (With one caveat: this only works if your priors are normalized.)
    """
    table.bayesian_update()
    table2 = table.reset()
    table2.likelihood = 1 / table.hypo
    table2.bayesian_update()
    assert table2.shape == (4, 5)
