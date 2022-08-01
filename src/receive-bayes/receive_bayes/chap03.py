"""
This is based on a notebook of example code from Think Bayes.
"""
import logging

from thinkbayes import Cdf
from thinkbayes.c03_distributions import Mean, MakePosterior, Train2
from thinkbayes.scripts.dice import Dice
from thinkbayes.scripts.train import Train


def dice_strategy(self, body: dict):
    # Here's what the update looks like:

    suite = Dice([4, 6, 8, 12, 20])
    suite.Update(6)
    suite.Print()
    # And here's what it looks like after more data:
    for roll in [6, 8, 7, 7, 5, 4]:
        suite.Update(roll)
    suite.Print()


def hypos_strategy(self, body: dict):
    hypos = range(1, 1001)  # But there are many more hypotheses
    suite = Train(hypos)
    suite.Update(60)
    Mean(suite)  # And here's how we can compute the posterior mean
    suite.Mean()  # Or we can just use the method


def MakePosterior_strategy(self, body: dict):
    # Let's run it with the same dataset and several uniform priors
    dataset = [30, 60, 90]
    for high in [500, 1000, 2000]:
        suite = MakePosterior(high, dataset)
        print(high, suite.Mean())


def Train2_strategy(self, body: dict):
    high = 100
    hypos = range(1, high + 1)
    suite1 = Train(hypos)  # uniform prior
    suite2 = Train2(hypos)  # power law prior

    dataset = [60]
    high = 1000

    constructors = [Train, Train2]
    labels = ["uniform", "power law"]

    for constructor, label in zip(constructors, labels):
        suite = MakePosterior(high, dataset, constructor)
        suite.label = label

    # The power law gives less prior probability to high values,
    # which yields lower posterior means, and less sensitivity to the upper bound.

    dataset = [30, 60, 90]

    for high in [500, 1000, 2000]:
        suite = MakePosterior(high, dataset, Train2)
        print(high, suite.Mean())

    # ## Credible intervals
    # To compute credible intervals, we can use the `Percentile` method on the posterior.
    # If you have to compute more than a few percentiles, it is more efficient to compute a CDF.
    # Also, a CDF can be a better way to visualize distributions.

    hypos = range(1, 1001)
    suite = Train(hypos)
    suite.Update(60)
    logging.info("%r", f"suite.percentile(5) = {suite.Percentile(5)}")
    logging.info("%r", f"suite.percentile(95) = {suite.Percentile(95)}")

    cdf = Cdf(suite)

    logging.info("%r", f"cdf.percentile(5) = {cdf.Percentile(5)}")
    logging.info("%r", f"cdf.percentile(95) = {cdf.Percentile(95)}")


def dice_problem_strategy(self, body: dict):
    """
    ## The Dice problem
    Suppose I have a box of dice that contains a 4-sided die, a 6-sided
    die, an 8-sided die, a 12-sided die, and a 20-sided die.

    I select a die from the box at random, roll it, and get a 6.
    What is the probability that I rolled each die?

    The `Dice` class inherits `Update` and provides `Likelihood`
    """
    suite = Dice([4, 6, 8, 12, 20])
    suite.Update(6)
    suite.Print()

    for roll in [6, 8, 7, 7, 5, 4]:  # after more data
        suite.Update(roll)

    suite.Print()


def train_problem_strategy(self, body: dict):
    """
    ## The train problem
    The Train problem has the same likelihood as the Dice problem.
    But there are many more hypotheses
    """
    hypos = range(1, 1001)
    suite = Train(hypos)
    suite.Update(60)

    Mean(suite)  # posterior mean
    suite.Mean()  # Or we can just use the method


def sensitivity_strategy(self, body: dict):
    """
    ## Sensitivity to the prior
    Here's a function that solves the train problem for different priors and data
    Let's run it with the same dataset and several uniform priors
    The results are quite sensitive to the prior, even with several observations.
    """
    dataset = [30, 60, 90]

    for high in [500, 1000, 2000]:
        suite = MakePosterior(high, dataset)
        print(high, suite.Mean())

    ## Power law prior

    dataset = [60]
    high = 1000

    constructors = [Train, Train2]  # Now let's see what the posteriors look like after observing one train.
    labels = ["uniform", "power law"]

    for constructor, label in zip(constructors, labels):
        suite = MakePosterior(high, dataset, constructor)
        suite.label = label

    dataset = [30, 60, 90]

    for high in [500, 1000, 2000]:
        suite = MakePosterior(high, dataset, Train2)
        print(high, suite.Mean())

    hypos = range(1, 1001)
    suite = Train(hypos)
    suite.Update(60)

    logging.info("%r", f"suite.percentile(5) = {suite.Percentile(5)}")
    logging.info("%r", f"suite.percentile(95) = {suite.Percentile(95)}")

    cdf = Cdf(suite)

    logging.info("%r", f"cdf.percentile(5) = {cdf.Percentile(5)}")
    logging.info("%r", f"cdf.percentile(95) = {cdf.Percentile(95)}")


def exercise_strategy(self, body: dict):
    """
    To write a likelihood function for the locomotive problem, we had
    to answer this question:  "If the railroad has `N` locomotives, what
    is the probability that we see number 60?"

    The answer depends on what sampling process we use when we observe the
    locomotive.  In the book, I resolved the ambiguity by specifying
    that there is only one train-operating company (or only one that we
    care about).

    But suppose instead that there are many companies with different
    numbers of trains.  And suppose that you are equally likely to see any
    train operated by any company.
    In that case, the likelihood function is different because you
    are more likely to see a train operated by a large company.

    As an exercise, implement the likelihood function for this variation
    of the locomotive problem, and compare the results.

    Solution

    Suppose Company A has N trains and all other companies have M.
    The chance that we would observe one of Company A's trains is $N/(N+M)$.
    Given that we observe one of Company A's trains, the chance that we
    observe number 60 is $1/N$ for $N \ge 60$.

    The product of these probabilities is $1/(N+M)$, which is just the
    probability of observing any given train.

    If N<<M, this converges to a constant, which means that all value of $N$
    have the same likelihood, so we learn nothing about how many trains
    Company A has.

    If N>>M, this converges to $1/N$, which is what we saw in the previous
    solution.

    More generally, if M is unknown, we would need a prior distribution for
    M, then we can do a two-dimensional update, and then extract the posterior
    distribution for N.

    We'll see how to do that soon.
    """
    pass
