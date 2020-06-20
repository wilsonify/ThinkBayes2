"""
Think Bayes: Chapter 3
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import thinkplot
from thinkbayes import Hist, Pmf, Suite, Cdf


def test_dice_problem():
    # ## The Dice problem
    #
    # Suppose I have a box of dice that contains a 4-sided die, a 6-sided
    # die, an 8-sided die, a 12-sided die, and a 20-sided die.
    #
    # I select a die from the box at random, roll it, and get a 6.
    # What is the probability that I rolled each die?
    #
    # The `Dice` class inherits `Update` and provides `Likelihood`

    class Dice(Suite):
        def Likelihood(self, data, hypo):
            if hypo < data:
                return 0
            else:
                return 1 / hypo

    # Here's what the update looks like:

    suite = Dice([4, 6, 8, 12, 20])
    suite.Update(6)
    suite.Print()

    # And here's what it looks like after more data:

    for roll in [6, 8, 7, 7, 5, 4]:
        suite.Update(roll)

    suite.Print()

    # ## The train problem
    #
    # The Train problem has the same likelihood as the Dice problem.

    class Train(Suite):
        def Likelihood(self, data, hypo):
            if hypo < data:
                return 0
            else:
                return 1 / hypo

    # But there are many more hypotheses

    hypos = range(1, 1001)
    suite = Train(hypos)
    suite.Update(60)

    # Here's what the posterior looks like

    thinkplot.Pdf(suite)

    # And here's how we can compute the posterior mean

    def Mean(suite):
        total = 0
        for hypo, prob in suite.Items():
            total += hypo * prob
        return total

    Mean(suite)

    # Or we can just use the method

    suite.Mean()

    # ## Sensitivity to the prior
    #
    # Here's a function that solves the train problem for different priors and data

    def MakePosterior(high, dataset, constructor=Train):
        """Solves the train problem.

        high: int maximum number of trains
        dataset: sequence of observed train numbers
        constructor: function used to construct the Train object

        returns: Train object representing the posterior suite
        """
        hypos = range(1, high + 1)
        suite = constructor(hypos)

        for data in dataset:
            suite.Update(data)

        return suite

    # Let's run it with the same dataset and several uniform priors

    dataset = [30, 60, 90]

    for high in [500, 1000, 2000]:
        suite = MakePosterior(high, dataset)
        print(high, suite.Mean())

    # The results are quite sensitive to the prior, even with several observations.

    # ## Power law prior
    #
    # Now let's try it with a power law prior.

    class Train2(Train):
        def __init__(self, hypos, alpha=1.0):
            Pmf.__init__(self)
            for hypo in hypos:
                self[hypo] = hypo ** (-alpha)
            self.Normalize()

    # Here's what a power law prior looks like, compared to a uniform prior

    high = 100
    hypos = range(1, high + 1)
    suite1 = Train(hypos)
    suite2 = Train2(hypos)
    thinkplot.Pdf(suite1)
    thinkplot.Pdf(suite2)

    # Now let's see what the posteriors look like after observing one train.

    dataset = [60]
    high = 1000

    thinkplot.PrePlot(num=2)

    constructors = [Train, Train2]
    labels = ["uniform", "power law"]

    for constructor, label in zip(constructors, labels):
        suite = MakePosterior(high, dataset, constructor)
        suite.label = label
        thinkplot.Pmf(suite)

    thinkplot.Config(xlabel="Number of trains", ylabel="Probability")

    # The power law gives less prior probability to high values, which yields lower posterior means, and less sensitivity to the upper bound.

    dataset = [30, 60, 90]

    for high in [500, 1000, 2000]:
        suite = MakePosterior(high, dataset, Train2)
        print(high, suite.Mean())

    # ## Credible intervals
    #
    # To compute credible intervals, we can use the `Percentile` method on the posterior.

    hypos = range(1, 1001)
    suite = Train(hypos)
    suite.Update(60)

    suite.Percentile(5), suite.Percentile(95)

    # If you have to compute more than a few percentiles, it is more efficient to compute a CDF.
    #
    # Also, a CDF can be a better way to visualize distributions.

    cdf = Cdf(suite)
    thinkplot.Cdf(cdf)
    thinkplot.Config(
        xlabel="Number of trains", ylabel="Cumulative Probability", legend=False
    )

    # `Cdf` also provides `Percentile`

    cdf.Percentile(5), cdf.Percentile(95)

    # ## Exercises

    # **Exercise:** To write a likelihood function for the locomotive problem, we had
    # to answer this question:  "If the railroad has `N` locomotives, what
    # is the probability that we see number 60?"
    #
    # The answer depends on what sampling process we use when we observe the
    # locomotive.  In the book, I resolved the ambiguity by specifying
    # that there is only one train-operating company (or only one that we
    # care about).
    #
    # But suppose instead that there are many companies with different
    # numbers of trains.  And suppose that you are equally likely to see any
    # train operated by any company.
    # In that case, the likelihood function is different because you
    # are more likely to see a train operated by a large company.
    #
    # As an exercise, implement the likelihood function for this variation
    # of the locomotive problem, and compare the results.

    # Solution

    # Suppose Company A has N trains and all other companies have M.
    # The chance that we would observe one of Company A's trains is $N/(N+M)$.
    # Given that we observe one of Company A's trains, the chance that we
    # observe number 60 is $1/N$ for $N \ge 60$.

    # The product of these probabilities is $1/(N+M)$, which is just the
    # probability of observing any given train.

    # If N<<M, this converges to a constant, which means that all value of $N$
    # have the same likelihood, so we learn nothing about how many trains
    # Company A has.

    # If N>>M, this converges to $1/N$, which is what we saw in the previous
    # solution.

    # More generally, if M is unknown, we would need a prior distribution for
    # M, then we can do a two-dimensional update, and then extract the posterior
    # distribution for N.

    # We'll see how to do that soon.
