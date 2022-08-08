"""
Think Bayes solutions: Chapter 4
This notebook presents solutions to exercises in Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes.c04_proportions import (
    Euro,
    TrianglePrior,
    Beta2,
    Euro2
)


def euro_strategy(self, body: dict):
    # We can make a uniform prior and update it with 140 heads and 110 tails:
    prior_str = body['prior']
    if prior_str == 'uniform':
        prior = range(0, 101)
    if prior_str == 'beta':
        precount_heads = body['precount_heads']  # 100
        precount_tails = body['precount_tails']  # 100
        prior = Beta2(precount_heads, precount_tails, label="beta")
    if prior_str == 'triangle':
        prior = TrianglePrior()

    suite = Euro2(prior)
    heads = body['heads']
    tails = body['tails']
    dataset = "H" * heads + "T" * tails
    for data in dataset:
        suite.Update(data)
    low, high = suite.CredibleInterval(90)
    result = dict(
        mean=suite.Mean(),
        median=suite.Percentile(50),
        maximum_aposteori_probability=suite.MAP(),  # The peak of the posterior
        creditable_low=low,
        creditable_high=high
    )
    self.publish(result)
