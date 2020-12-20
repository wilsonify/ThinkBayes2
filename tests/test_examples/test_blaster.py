"""
The Alien Blaster problem
This notebook presents solutions to exercises in Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import logging

import numpy as np
import pytest
from numpy.random import random
from scipy import stats
from thinkbayes import Hist, Pmf, Suite, Beta
from thinkbayes import make_binomial_pmf
from thinkbayes import make_mixture
from thinkbayes import thinkplot


def flip(p):
    return random() < p


def simulate_shots(n, p):
    return np.random.binomial(n, p)


class AlienBlaster(Suite):
    """
    Now suppose the new ultra-secret Alien Blaster 10K is being tested.
    In a press conference, an EDF general reports that the new design has been tested twice,
    taking two shots during each test.
    The results of the test are confidential, so the general won't say how many targets were hit, but they report:
    "The same number of targets were hit in the two tests,
    so we have reason to think this new design is consistent."

    Write a class called `AlienBlaster` that inherits from `Suite`
    and provides a likelihood function that takes this
    data -- two shots and a tie -- and computes the likelihood of the data for each hypothetical value of $x$.
    If you would like a challenge, write a version that works for any number of shots.
    """

    def likelihood(self, data, hypo):
        """Computes the likelihood of data under hypo.

        data: number of shots they took
        hypo: probability of a hit, p
        """
        n = data
        x = hypo

        # specific version for n=2 shots
        likes = [x ** 4, (1 - x) ** 4, (2 * x * (1 - x)) ** 2]
        logging.debug("%r", f"for n=2 shots likes={likes}")

        # general version for any n shots
        likes = [stats.binom.pmf(k, n, x) ** 2 for k in range(n + 1)]

        return np.sum(likes)


@pytest.fixture(name="prior")
def prior_fixture():
    return Beta(2, 3)


def test_blaster(prior):
    """
    Part One
    In preparation for an alien invasion,
    the Earth Defense League has been working on new missiles to shoot down space invaders.
    Of course, some missile designs are better than others;
    let's assume that each design has some probability of hitting an alien ship, $x$.

    Based on previous tests, the distribution of $x$ in the population of designs is well-modeled by
    a beta distribution with parameters $\alpha=2$ and $\beta=3$.
    What is the average missile's probability of shooting down an alien?

    :return:
    """

    thinkplot.plot_pdf_line(prior.make_pmf())
    assert prior.mean() == 0.4


def test_blaster2():
    """
    # In its first test, the new Alien Blaster 9000 takes 10 shots and hits 2 targets.
    # Taking into account this data, what is the posterior distribution of $x$ for this missile?
    # What is the value in the posterior with the highest probability, also known as the MAP?

    :return:
    """

    posterior = Beta(3, 2)
    posterior.update((2, 8))
    assert posterior.map() == pytest.approx(0.3, abs=0.01)


def test_blaster31(prior):
    """
    If we start with a uniform prior, we can see what the likelihood function looks like:
    A tie is most likely if they are both terrible shots or both very good.
    Is this data good or bad; that is, does it increase or decrease your estimate of $x$ for the Alien Blaster 10K?
    Now let's run it with the specified prior and
    see what happens when we multiply the convex prior and the concave posterior:

    :return:
    """

    pmf = Beta(1, 1).make_pmf()
    blaster = AlienBlaster(pmf)
    blaster.update(2)
    thinkplot.plot_pdf_line(blaster)
    assert prior.mean() < blaster.mean()


def test_blaster32(prior):
    """
    Now let's run it with the specified prior and
    see what happens when we multiply the convex prior and the concave posterior:

    :return:
    """
    pmf = Beta(2, 3).make_pmf()
    blaster = AlienBlaster(pmf)
    blaster.update(2)
    thinkplot.plot_pdf_line(blaster)

    assert (
            prior.mean() > blaster.mean()
    )  # The posterior mean and MAP are lower than in the prior.


def test_blaster35(prior):
    """
    A tie is most likely if they are both terrible shots or both very good.
    Is this data good or bad; that is, does it increase or decrease your estimate of $x$ for the Alien Blaster 10K?
    Now let's run it with the specified prior and
    see what happens when we multiply the convex prior and the concave posterior:

    So if we learn that the new design is "consistent", it is more likely to be consistently bad (in this case).

    :return:
    """
    pmf = Beta(2, 3).make_pmf()
    blaster = AlienBlaster(pmf)
    blaster.update(2)
    thinkplot.plot_pdf_line(blaster)

    assert (
            prior.map() > blaster.map()
    )  # The posterior mean and MAP are lower than in the prior.


def test_blaster4():
    """
    Part Two

    Suppose we
    have we have a stockpile of 3 Alien Blaster 9000s and 7 Alien
    Blaster 10Ks.  After extensive testing, we have concluded that
    the AB9000 hits the target 30% of the time, precisely, and the
    AB10K hits the target 40% of the time.

    If I grab a random weapon from the stockpile and shoot at 10 targets,
    what is the probability of hitting exactly 3?  Again, you can write a
    number, mathematical expression, or Python code.

    :return:
    """

    k_cont = 3
    n_const = 10
    x1 = 0.3
    x2 = 0.4

    result = 0.3 * stats.binom.pmf(k_cont, n_const, x1) + 0.7 * stats.binom.pmf(
        k_cont, n_const, x2
    )
    assert result


def test_blaster5():
    """
    The answer is a value drawn from the mixture of the two distributions.
    Continuing the previous problem, let's estimate the distribution
    of `k`, the number of successful shots out of 10.

    1. Write a few lines of Python code to simulate choosing a random weapon and firing it.
    2. Write a loop that simulates the scenario and generates random values of `k` 1000 times.
    3. Store the values of `k` you generate and plot their distribution.

    :return:
    """
    n_const = 10
    x1 = 0.3
    x2 = 0.4

    ks = []
    for _ in range(1000):
        if flip(0.3):
            k_success = simulate_shots(n_const, x1)
        else:
            k_success = simulate_shots(n_const, x2)
        ks.append(k_success)

    pmf = Pmf(ks)
    thinkplot.plot_hist_bar(pmf)  # Here's what the distribution looks like.
    assert len(ks) == 1000


def test_blaster55():
    """
    The answer is a value drawn from the mixture of the two distributions.
    Continuing the previous problem, let's estimate the distribution
    of `k`, the number of successful shots out of 10.

    1. Write a few lines of Python code to simulate choosing a random weapon and firing it.
    2. Write a loop that simulates the scenario and generates random values of `k` 1000 times.
    3. Store the values of `k` you generate and plot their distribution.

    :return:
    """
    n_const = 10
    x1 = 0.3
    x2 = 0.4

    ks = []
    for _ in range(1000):
        if flip(0.3):
            k_success = simulate_shots(n_const, x1)
        else:
            k_success = simulate_shots(n_const, x2)
        ks.append(k_success)

    pmf = Pmf(ks)
    thinkplot.plot_hist_bar(pmf)  # Here's what the distribution looks like.

    assert np.mean(ks) == pytest.approx(3.7, abs=0.2)  # The mean should be near 3.7.


def test_blaster6():
    """
    We can run this simulation more efficiently using NumPy.
    First we generate a sample of `xs`:

    :return:
    """
    n_const = 10
    x1 = 0.3
    x2 = 0.4

    xs = np.random.choice(a=[x1, x2], p=[0.3, 0.7], size=1000)
    Hist(xs)

    ks = np.random.binomial(n_const, xs)  # Then for each `x` we generate a `k`:

    pmf = Pmf(ks)
    thinkplot.plot_hist_bar(pmf)  # And the results look similar.
    assert np.mean(ks) == pytest.approx(3.7, abs=0.2)


def test_blaster7():
    """
    One more way to do the same thing is to make a meta-Pmf, which contains the two binomial `Pmf` objects:

    :return:
    """
    n_const = 10
    x1 = 0.3
    x2 = 0.4

    pmf1 = make_binomial_pmf(n_const, x1)
    pmf2 = make_binomial_pmf(n_const, x2)

    metapmf = Pmf({pmf1: 0.3, pmf2: 0.7})
    metapmf.print()

    ks = [
        metapmf.random().random() for _ in range(1000)
    ]  # Here's how we can draw samples from the meta-Pmf:

    pmf = Pmf(ks)
    thinkplot.plot_hist_bar(pmf)  # And here are the results, one more time:
    assert np.mean(ks) == pytest.approx(3.7, abs=0.1)


def test_blaster8():
    """
    This result, which we have estimated three ways, is a predictive distribution, based on our uncertainty about `x`.    
    We can compute the mixture analytically using `thinkbayes.MakeMixture`:

        def MakeMixture(metapmf, label='mix'):
            '''Make a mixture distribution.

            Args:
              metapmf: PMF that maps from PMFs to probabilities.
              label: string label for the new Pmf.

            Returns: Pmf object.
            '''
            mix = Pmf(label=label)
            for pmf, p1 in metapmf.Items():
                for k, p2 in pmf.Items():
                    mix[k] += p1 * p2
            return mix

    
    The outer loop iterates through the PMFs; the inner loop iterates through the items.
    
    So `p1` is the probability of choosing a particular Pmf; `p2` is the probability of choosing a value from the Pmf.
    
    In the example, each Pmf is associated with a value of `x` (probability of hitting a target).
    The inner loop enumerates the values of `k` (number of targets hit after 10 shots).

    :return:
    """
    n_const = 10
    x1 = 0.3
    x2 = 0.4
    pmf1 = make_binomial_pmf(n_const, x1)
    pmf2 = make_binomial_pmf(n_const, x2)
    metapmf = Pmf({pmf1: 0.3, pmf2: 0.7})
    mix = make_mixture(metapmf)
    thinkplot.plot_hist_bar(mix)
    assert mix.mean() == pytest.approx(3.7, abs=0.2)
