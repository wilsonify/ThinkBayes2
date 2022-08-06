"""
This is based on a notebook of example code from Think Bayes.
"""

from thinkbayes import Cdf
from thinkbayes.c03_distributions import MakePosterior, Train2
from thinkbayes.scripts.dice import Dice


def dice_strategy(self, body: dict):
    suite = Dice([1, 2, 3, 4, 5, 6])
    observations = body["observations"]
    for roll in observations:
        suite.Update(roll)
    result = suite.GetDict()
    self.publish(result)


def train_strategy(self, body: dict):
    # The power law gives less prior probability to high values,
    # which yields lower posterior means, and less sensitivity to the upper bound.
    suite = MakePosterior(
        high=1001,
        dataset=body["observations"],
        constructor=Train2  # power law prior
    )
    cdf = Cdf(suite)
    result = dict(
        low=cdf.Percentile(5),
        most_likely=suite.Mode(),
        high=cdf.Percentile(95)
    )
    self.publish(result)
