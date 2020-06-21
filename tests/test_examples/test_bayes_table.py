"""
Bayesian updates using the table method

This notebook demonstrates a way of doing simple Bayesian updates using the table method,
with a Pandas DataFrame as the table.

Copyright 2018 Allen Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy as np
import pandas as pd
import pytest


class BayesTable(pd.DataFrame):
    """
    As an example, I'll use the "cookie problem", which is a version of a classic probability "urn problem".
    Suppose there are two bowls of cookies.
    * Bowl #1 has 10 chocolate and 30 vanilla.
    * Bowl #2 has 20 of each.

    You choose a bowl at random, and then pick a cookie at random.
    The cookie turns out to be vanilla.
    What is the probability that the bowl you picked from is Bowl #1?

    The BayesTable class
    Here's the class that represents a Bayesian table.
    """

    def __init__(self, hypo, prior=1):
        columns = ["hypo", "prior", "likelihood", "unnorm", "posterior"]
        super().__init__(columns=columns)
        self.hypo = hypo
        self.prior = prior
        self.posterior = None
        self.unnorm = None

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

    @property
    def _constructor_expanddim(self):
        raise NotImplementedError("Not supported for BayesTable!")


@pytest.fixture(name="table")
def table_fixture():
    return BayesTable(["Bowl1", "Bowl2"])


def test_bayestable(table):
    """
    Here's an instance that represents the two hypotheses: you either chose from Bowl 1 or Bowl 2:
    Since we didn't specify prior probabilities, the default value is equal priors for all hypotheses.
    Now we can specify the likelihoods:
    * The likelihood of getting a vanilla cookie from Bowl 1 is 3/4.
    * The likelihood of getting a vanilla cookie from Bowl 2 is 1/2.
    Here's how we plug the likelihoods in:
    """
    table.likelihood = [3 / 4, 1 / 2]
    assert table.shape == (2, 5)


def test_bayestable2(table):
    """
    The next step is to multiply the priors by the likelihoods, which yields the unnormalized posteriors.
    :return:
    """
    table.likelihood = [3 / 4, 1 / 2]
    table.mult()
    assert table.shape == (2, 5)


def test_bayestable3(table):
    """
    Now we can compute the normalized posteriors; `norm` returns the normalization constant.
    :return:
    """
    table.likelihood = [3 / 4, 1 / 2]
    table.mult()
    table.norm()
    assert table.shape == (2, 5)


def test_bayestable4(table):
    """
    We can read the posterior probabilities from the last column: the probability that you chose from Bowl 1 is 60%.
    Resetting
    Suppose you put the first cookie back, stir the bowl, and select another cookie from the same bowl.
    If this second cookie is chocolate, what is the probability, now, that you are drawing from Bowl 1?
    To solve this problem, we want a new table where the priors in the new table are the posteriors from the old table.
    That's what the `reset` method computes:

    :return:
    """
    table.likelihood = [3 / 4, 1 / 2]
    table.mult()
    table.norm()
    table2 = table.reset()
    table2.likelihood = [
        1 / 4,
        1 / 2,
    ]  # Here are the likelihoods for the second update.
    table2.bayesian_update()  # We could run `mult` and `norm` again, or run `update`, which does both steps.
    assert table2.shape == (2, 5)
