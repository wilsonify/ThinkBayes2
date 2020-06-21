"""
This notebook presents code and exercises from Think Bayes: Chapter 11
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

from thinkbayes import Suite


class Euro(Suite):
    """
    The Euro problem
    Here's a more efficient version of the Euro class that takes the dataset in a more compact form
    and uses the binomial distribution (ignoring the binomial coefficient because it does not depend on `x`).

    Represents hypotheses about the probability of heads.
    """

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: integer value of x, the probability of heads (0-100)
        data: tuple of (number of heads, number of tails)
        """
        x = hypo / 100.0
        heads, tails = data
        like = x ** heads * (1 - x) ** tails
        return like


def SuiteLikelihood(suite, data):
    """
    Under this interpretation, the data are in favor of "biased", but very weak.
    More generally, if "biased" refers to a range of possibilities with different probabilities,
    the total likelihood of the data is the weighted sum:


    Computes the weighted average of likelihoods for sub-hypotheses.

    suite: Suite that maps sub-hypotheses to probability
    data: some representation of the data

    returns: float likelihood
    """
    total = 0
    for hypo, prob in suite.items():
        like = suite.likelihood(data, hypo)
        total += prob * like
    return total


def TrianglePrior():
    """
    By that definition, the data are evidence against the biased hypothesis, with K=2.
    But maybe a triangle prior is a better model of what "biased" means.

    Makes a Suite with a triangular prior.
    """
    suite = Euro()
    for x in range(0, 51):
        suite.set(x, x)
    for x in range(51, 101):
        suite.set(x, 100 - x)
    suite.normalize()
    return suite


def test_euro():
    """
    If we know the coin is fair, we can evaluate the likelihood of the data directly.
    :return:
    """

    data = 140, 110

    suite = Euro()
    like_f = suite.likelihood(data, 50)
    print("p(D|F)", like_f)
    # -

    # If we cheat an pretend that the alternative hypothesis is exactly the observed proportion, we can compute the likelihood of the data and the likelihood ratio, relative to the fair coin.

    actual_percent = 100.0 * 140 / 250
    likelihood = suite.likelihood(data, actual_percent)
    print("p(D|B_cheat)", likelihood)
    print("p(D|B_cheat) / p(D|F)", likelihood / like_f)

    # Under this interpretation, the data are in favor of "biased", with K=6.  But that's a total cheat.
    #
    # Suppose we think "biased" means either 0.4 or 0.6, but we're not sure which.  The total likelihood of the data is the weighted average of the two likelihoods.

    like40 = suite.likelihood(data, 40)
    like60 = suite.likelihood(data, 60)
    likelihood = 0.5 * like40 + 0.5 * like60
    print("p(D|B_two)", likelihood)
    print("p(D|B_two) / p(D|F)", likelihood / like_f)

    # Here's what it looks like if "biased" means "equally likely to be any value between 0 and 1".

    b_uniform = Euro(range(0, 101))
    b_uniform.remove(50)
    b_uniform.normalize()
    likelihood = SuiteLikelihood(b_uniform, data)
    print("p(D|B_uniform)", likelihood)
    print("p(D|B_uniform) / p(D|F)", likelihood / like_f)

    # Here's what it looks like:

    b_tri = TrianglePrior()
    b_tri.remove(50)
    b_tri.normalize()
    likelihood = b_tri.update(data)
    print("p(D|B_tri)", likelihood)
    print("p(D|B_tri) / p(D|F)", likelihood / like_f)

    # By the triangle definition of "biased", the data are very weakly in favor of "fair".
    #
    # ## Normalizing constant
    #
    # We don't really need the SuiteLikelihood function, because `Suite.Update`
    # already computes the total probability of the data, which is the normalizing constant.

    likelihood = SuiteLikelihood(b_uniform, data)
    logging.info("%r", f"likelihood = {likelihood}")

    euro = Euro(b_uniform)
    euro.update(data)

    likelihood = SuiteLikelihood(b_tri, data)
    logging.info("%r", f"likelihood = {likelihood}")


    euro = Euro(b_tri)
    euro.update(data)

    # This observation is the basis of hierarchical Bayesian models, of which this solution to the Euro problem is a simple example.
