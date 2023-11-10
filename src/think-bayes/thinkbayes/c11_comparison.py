from thinkbayes import Suite


class Euro(Suite):
    """
    The Euro problem
    Here's a more efficient version of the Euro class that takes the dataset in a more compact form
    and uses the binomial distribution (ignoring the binomial coefficient because it does not depend on `x`).

    Represents hypotheses about the probability of heads.
    """

    def Likelihood(self, data, hypo):
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
    for hypo, prob in suite.Items():
        like = suite.Likelihood(data, hypo)
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
        suite.Set(x, x)
    for x in range(51, 101):
        suite.Set(x, 100 - x)
    suite.Normalize()
    return suite
