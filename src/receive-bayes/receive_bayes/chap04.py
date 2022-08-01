"""
This is based on a notebook from Think Bayes : Chapter 4.
"""

from thinkbayes.c04_proportions import Euro, UniformPrior, TrianglePrior, RunUpdate, Euro2, Beta


def test_uniform():
    # We can make a uniform prior and update it with 140 heads and 110 tails:

    # +
    suite = Euro(range(0, 101))
    dataset = "H" * 140 + "T" * 110

    for data in dataset:
        suite.Update(data)
    # -

    # And here's what the posterior looks like.


    # We can summarize the posterior several ways, including the mean:

    suite.Mean()

    # Median:

    suite.Percentile(50)

    # The peak of the posterior, known as the Maximum Aposteori Probability (MAP)

    suite.MAP()

    # And a 90% credible interval

    suite.CredibleInterval(90)

    # We can look up a particular value in the posterior PMF, but the result doesn't mean much, because we could have divided the range (0-100) into as many pieces as we like, and the result would be different.

    suite.Prob(50)


def test_priors():
    triangle = TrianglePrior()
    uniform = UniformPrior()
    suites = [triangle, uniform]

    for suite in suites:
        RunUpdate(suite)

    # The results are almost identical; the remaining difference is unlikely to matter in practice.


def test_euro2():
    # I left out the binomial coefficient ${n}\choose{k}$ because it does not depend on `p`, so it's the same for all hypotheses.

    suite = Euro2(range(0, 101))
    dataset = 140, 110
    suite.Update(dataset)

    # Here's what the posterior looks like.


def test_beta():
    # -

    # Here's how we use it.

    beta = Beta()
    beta.Update((140, 110))
    beta.Mean()

    # And here's the posterior.

    # Amazing, no?

    # **Exercise:**
    # One way to construct priors is to make a Beta distribution and
    # adjust the parameters until it has the shape you want.
    # Then when you do an update, the data get added to the parameters of the prior.
    # Since the parameters of the prior play the same mathematical role as the data,
    # they are sometimes called "precounts".
    #
    # Suppose you believe that most coins are fair or
    # unlikely to deviate from 50% by more than a few percentage points.
    # Construct a prior that captures this belief and update it with the Euro data.
    # How much effect does it have on the posterior, compared to the uniform prior?
    #
    # Hint: A Beta distribution with parameters `(1, 1)` is uniform from 0 to 1.

    # +
    # Solution goes here

    # +
    # Solution goes here

    # +
    # Solution goes here

    # +
    # Solution goes here

    # +
    # Solution goes here
