"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from itertools import combinations

import numpy as np
from thinkbayes import Pmf, Suite
from thinkbayes import thinkplot


# ## Cats and rats and elephants
#
# Suppose there are six species that might be in a zoo: lions and tigers and bears, and cats and rats and elephants.  Every zoo has a subset of these species, and every subset is equally likely.
#
# One day we visit a zoo and see 3 lions, 2 tigers, and one bear.  Assuming that every animal in the zoo has an equal chance to be seen, what is the probability that the next animal we see is an elephant?
#
#
# ## Solution
#
# I'll start by enumerating all possible zoos with `itertools`.


def test_elephant():
    def power_set(s):
        n = len(s)
        for r in range(1, n + 1):
            for combo in combinations(s, r):
                yield "".join(combo)

    # Now we can enumerate only the zoos that are possible, given a set of animals known to be present.

    def enumerate_zoos(all_species, present):
        """Enumerate all zoos that contain `present`.

        all_species: sequence of all species
        present: sequence of species present

        yields: possible zoos
        """
        present = set(present)
        for combo in power_set(species):
            intersect = set(combo) & present
            if len(intersect) == len(present):
                yield len(combo), combo

    # Here are the possible zoos.

    species = "LTBCRE"
    present = "LTB"

    for n, zoo in enumerate_zoos(species, present):
        print(n, zoo)

    # To represent the prior and posterior distributions I'll use a hierarchical model with one Dirichlet object for each possible zoo.
    #
    # At the bottom of the hierarchy, it is easy to update each Dirichlet object just by adding the observed frequencies to the parameters.
    #
    # In order to update the top of the hierarchy, we need the total probability of the data for each hypothetical zoo.  When we do an update using grid algorithms, we get the probability of the data free, since it is the normalizing constant.
    #
    # But when we do an update using a conjugate distribution, we don't get the total probability of the data, and for a Dirichlet distribution it is not easy to compute.
    #
    # However, we can estimate it by drawing samples from the Dirichlet distribution, and then computing the probability of the data for each sample.

    class Dirichlet(object):
        """Represents a Dirichlet distribution.

        See http://en.wikipedia.org/wiki/Dirichlet_distribution
        """

        def __init__(self, n, conc=1, label=None):
            """Initializes a Dirichlet distribution.

            n: number of dimensions
            conc: concentration parameter (smaller yields more concentration)
            label: string label
            """
            if n < 2:
                raise ValueError("A Dirichlet distribution with " "n<2 makes no sense")

            self.n = n
            self.params = np.ones(n, dtype=np.float) * conc
            self.label = label if label is not None else "_nolegend_"

        def update(self, data):
            """Updates a Dirichlet distribution.

            data: sequence of observations, in order corresponding to params
            """
            m = len(data)
            self.params[:m] += data

        def random(self):
            """Generates a random variate from this distribution.

            Returns: normalized vector of fractions
            """
            p = np.random.gamma(self.params)
            return p / p.sum()

        def mean(self):
            """Array of means."""
            return self.params / self.params.sum()

    # Here's an example that represents a zoo with 4 animals.

    d4 = Dirichlet(4)

    # Here's a sample from it.

    p = d4.random()

    # Now we can compute the probability of the data, given these prevalences, using the multinomial distribution.

    from scipy.stats import multinomial

    data = [3, 2, 1, 0]
    m = sum(data)
    multinomial(m, p).pmf(data)

    # Since I only observed 3 species, and my hypothetical zoo has 4, I had to zero-pad the data.  Here's a function that makes that easier:

    def zero_pad(a, n):
        """Why does np.pad have to be so complicated?
        """
        res = np.zeros(n)
        res[: len(a)] = a
        return res

    # Here's an example:

    data = [3, 2, 1]
    zero_pad(data, 4)

    # Let's pull all that together.  Here's a function that estimates the total probability of the data by sampling from the dirichlet distribution:

    def sample_likelihood(dirichlet, data, iters=1000):
        """Estimate the total probability of the data.

        dirichlet: Dirichlet object
        data: array of observed frequencies
        iters: number of samples to draw
        """
        data = zero_pad(data, dirichlet.n)
        m = np.sum(data)
        likes = [multinomial(m, dirichlet.random()).pmf(data) for i in range(iters)]
        return np.mean(likes)

    # And here's an example:

    sample_likelihood(d4, data)

    # Now we're ready to solve the problem.
    #
    # Here's a Suite that represents the set of possible zoos.  The likelihood of any zoo is just the total probability of the data.

    class Zoo(Suite):
        def Likelihood(self, data, hypo):
            """
            data: sequence of counts
            hypo: Dirichlet object
            """
            return sample_likelihood(hypo, data)

    # We can construct the prior by enumerating the possible zoos.

    suite = Zoo(
        [
            Dirichlet(n, label="".join(zoo))
            for n, zoo in enumerate_zoos(species, present)
        ]
    )

    def print_zoos(suite):
        for d, p in suite.Items():
            print(p, d.label)

    print_zoos(suite)

    # We can update the top level of the hierarchy by calling `Update`

    suite.Update(data)

    # We have to update the bottom level explicitly.

    for hypo in suite:
        hypo.update(data)

    # Here's the posterior for the top level.

    print_zoos(suite)

    # Here's how we can get the posterior distribution of `n`, the number of species.

    pmf_n = Pmf()
    for d, p in suite.Items():
        pmf_n[d.n] += p

    # And here's what it looks like.

    thinkplot.Hist(pmf_n)
    print(pmf_n.Mean())
    thinkplot.decorate(xlabel="n", ylabel="PMF", title="Posterior distribution of n")

    # Now, to answer the question, we have to compute the posterior distribution of the prevalence of elephants.  Here's a function that computes it.

    def enumerate_posterior(suite):
        for d, p in suite.Items():
            mean = d.mean()
            index = d.label.find("E")
            p_elephant = 0 if index == -1 else mean[index]
            yield d, p, p_elephant

    # Here are the possible zoos, the posterior probability of each, and the conditional prevalence of elephants for each.

    for d, p, p_elephant in enumerate_posterior(suite):
        print(d.label, p, p_elephant)

    # Finally, we can use the law of total probability to compute the probability of seeing an elephant.

    total = np.sum(p * p_elephant for d, p, p_elephant in enumerate_posterior(suite))
