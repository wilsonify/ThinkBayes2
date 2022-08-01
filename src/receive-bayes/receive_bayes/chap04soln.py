"""
Think Bayes solutions: Chapter 4
This notebook presents solutions to exercises in Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes.c04_proportions import Euro3, Euro4, UniformPrior2, TrianglePrior2, RunUpdate2, Beta2


def test_euro_problem():
    # ## The Euro problem
    #
    # Here's a class that represents hypotheses about the probability a coin lands heads.

    # We can make a uniform prior and update it with 140 heads and 110 tails:

    suite = Euro3(range(0, 101))
    dataset = "H" * 140 + "T" * 110

    for data in dataset:
        suite.Update(data)

    # And here's what the posterior looks like.

    # We can summarize the posterior several ways, including the mean:

    suite.Mean()

    # Median:

    suite.Percentile(50)

    # The peak of the posterior, known as the Maximum Aposteori Probability (MAP)

    suite.MAP()

    # And a 90% credible interval

    suite.CredibleInterval(90)

    # We can look up a particular value in the posterior PMF,
    # but the result doesn't mean much,
    # because we could have divided the range (0-100) into as many pieces as we like,
    # and the result would be different.

    suite.Prob(50)

    # ## Different priors
    #
    # Let's see how that looks with different priors.
    #
    # Here's a function that makes a uniform prior:

    # Here's what they look like:

    triangle = TrianglePrior2()
    uniform = UniformPrior2()
    suites = [triangle, uniform]

    # If we update them both with the same data:

    for suite in suites:
        RunUpdate2(suite)

    # The results are almost identical; the remaining difference is unlikely to matter in practice.

    # ## The binomial likelihood function
    #
    # We can make the Euro class more efficient by computing the likelihood of the entire dataset at once,
    # rather than one coin toss at a time.
    #
    # If the probability of heads is p,
    # we can compute the probability of k=140 heads in n=250 tosses using the binomial PMF.

    # I left out the binomial coefficient ${n}\choose{k}$ because it does not depend on `p`,
    # so it's the same for all hypotheses.

    suite = Euro4(range(0, 101))
    dataset = 140, 110
    suite.Update(dataset)

    # Here's what the posterior looks like.

    # ## The Beta distribution
    #
    # The Beta distribution is a conjugate prior for the binomial likelihood function,
    # which means that if you start with a Beta distribution and update with a binomial likelihood,
    # the posterior is also Beta.
    #
    # Also, given the parameters of the prior and the data,
    # we can compute the parameters of the posterior directly.
    # The following class represents a Beta distribution and provides a constant-time Update method.

    # Here's how we use it.

    beta = Beta2()
    beta.Update((140, 110))
    beta.Mean()

    # And here's the posterior.

    # Amazing, no?

    # **Exercise:**
    # One way to construct priors is to make a Beta distribution
    # and adjust the parameters until it has the shape you want.
    # Then when you do an update, the data get added to the parameters of the prior.
    # Since the parameters of the prior play the same mathematical role as the data,
    # they are sometimes called "precounts".
    #
    # Suppose you believe that most coins are fair or unlikely
    # to deviate from 50% by more than a few percentage points.
    # Construct a prior that captures this belief and update it with the Euro data.
    # How much effect does it have on the posterior, compared to the uniform prior?
    #
    # Hint: A Beta distribution with parameters `(1, 1)` is uniform from 0 to 1.

    # Solution

    # Here's the uniform prior

    uniform = Beta2(1, 1, label="uniform")

    # Solution

    # And here's what it looks like after the update

    uniform.Update(dataset)

    # Solution

    # Here's a beta prior with precounts chosen to represent

    # background knowledge about coins.

    beta = Beta2(100, 100, label="beta")

    # Solution

    # And here's what it looks like after the update

    beta.Update(dataset)

    # Solution

    # Comparing the two, we see that the (more) informative
    # prior influences the location and spread of the
    # posterior.
