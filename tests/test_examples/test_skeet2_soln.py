"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np
from thinkbayes import Pmf, Beta
from thinkbayes import thinkplot


def test_comparing_distributions():
    # ## Comparing distributions
    #
    # Let's get back to the Kim Rhode problem from Chapter 4:
    #
    # > At the 2016 Summer Olympics in the Women's Skeet event, Kim Rhode faced Wei Meng in the bronze medal match. They each hit 15 of 25 targets, sending the match into sudden death. In the first round, both hit 1 of 2 targets. In the next two rounds, they each hit 2 targets. Finally, in the fourth round, Rhode hit 2 and Wei hit 1, so Rhode won the bronze medal, making her the first Summer Olympian to win an individual medal at six consecutive summer games.
    #
    # >But after all that shooting, what is the probability that Rhode is actually a better shooter than Wei? If the same match were held again, what is the probability that Rhode would win?
    #
    # I'll start with a uniform distribution for `x`, the probability of hitting a target, but we should check whether the results are sensitive to that choice.
    #
    # First I create a Beta distribution for each of the competitors, and update it with the results.

    rhode = Beta(1, 1, label="Rhode")
    rhode.update((22, 11))

    wei = Beta(1, 1, label="Wei")
    wei.update((21, 12))

    # Based on the data, the distribution for Rhode is slightly farther right than the distribution for Wei, but there is a lot of overlap.

    thinkplot.plot_pdf_line(rhode.make_pmf())
    thinkplot.plot_pdf_line(wei.make_pmf())
    thinkplot.config_plot(xlabel="x", ylabel="Probability")

    # To compute the probability that Rhode actually has a higher value of `p`, there are two options:
    #
    # 1. Sampling: we could draw random samples from the posterior distributions and compare them.
    #
    # 2. Enumeration: we could enumerate all possible pairs of values and add up the "probability of superiority".
    #
    # I'll start with sampling.  The Beta object provides a method that draws a random value from a Beta distribution:

    iters = 1000
    count = 0
    for _ in range(iters):
        x1 = rhode.random()
        x2 = wei.random()
        if x1 > x2:
            count += 1

    logging.info("%r", f"count / iters = {count / iters}")

    # `Beta` also provides `Sample`, which returns a NumPy array, so we an perform the comparisons using array operations:

    rhode_sample = rhode.sample(iters)
    wei_sample = wei.sample(iters)
    np.mean(rhode_sample > wei_sample)

    # The other option is to make `Pmf` objects that approximate the Beta distributions, and enumerate pairs of values:

    def ProbGreater(pmf1, pmf2):
        total = 0
        for x1, prob1 in pmf1.items():
            for x2, prob2 in pmf2.items():
                if x1 > x2:
                    total += prob1 * prob2
        return total

    pmf1 = rhode.make_pmf(1001)
    pmf2 = wei.make_pmf(1001)
    ProbGreater(pmf1, pmf2)

    pmf1.prob_greater(pmf2)

    pmf1.prob_less(pmf2)

    # **Exercise:** Run this analysis again with a different prior and see how much effect it has on the results.

    # ## Simulation
    #
    # To make predictions about a rematch, we have two options again:
    #
    # 1. Sampling.  For each simulated match, we draw a random value of `x` for each contestant, then simulate 25 shots and count hits.
    #
    # 2. Computing a mixture.  If we knew `x` exactly, the distribution of hits, `k`, would be binomial.  Since we don't know `x`, the distribution of `k` is a mixture of binomials with different values of `x`.
    #
    # I'll do it by sampling first.

    
    def flip(p):
        return np.random.random() < p

    # `flip` returns True with probability `p` and False with probability `1-p`
    #
    # Now we can simulate 1000 rematches and count wins and losses.

    iters = 1000
    wins = 0
    losses = 0

    for _ in range(iters):
        x1 = rhode.random()
        x2 = wei.random()

        count1 = count2 = 0
        for _ in range(25):
            if flip(x1):
                count1 += 1
            if flip(x2):
                count2 += 1

        if count1 > count2:
            wins += 1
        if count1 < count2:
            losses += 1
    logging.info("%r", f"wins / iters, losses / iters = {wins / iters, losses / iters}")

    # Or, realizing that the distribution of `k` is binomial, we can simplify the code using NumPy:

    rhode_rematch = np.random.binomial(25, rhode_sample)
    thinkplot.plot_hist_bar(Pmf(rhode_rematch))

    wei_rematch = np.random.binomial(25, wei_sample)
    np.mean(rhode_rematch > wei_rematch)

    np.mean(rhode_rematch < wei_rematch)

    # Alternatively, we can make a mixture that represents the distribution of `k`, taking into account our uncertainty about `x`:

    from thinkbayes import make_binomial_pmf

    def MakeBinomialMix(pmf, label=""):
        mix = Pmf(label=label)
        for x, prob in pmf.items():
            binom = make_binomial_pmf(n=25, p=x)
            for k, p in binom.items():
                mix[k] += prob * p
        return mix

    rhode_rematch = MakeBinomialMix(rhode.make_pmf(), label="Rhode")
    wei_rematch = MakeBinomialMix(wei.make_pmf(), label="Wei")
    thinkplot.plot_pdf_line(rhode_rematch)
    thinkplot.plot_pdf_line(wei_rematch)
    thinkplot.config_plot(xlabel="hits")

    rhode_rematch.prob_greater(wei_rematch), rhode_rematch.prob_less(wei_rematch)

    # Alternatively, we could use MakeMixture:

    from thinkbayes import make_mixture

    def MakeBinomialMix2(pmf):
        binomials = Pmf()
        for x, prob in pmf.items():
            binom = make_binomial_pmf(n=25, p=x)
            binomials[binom] = prob
        return make_mixture(binomials)

    # Here's how we use it.

    rhode_rematch = MakeBinomialMix2(rhode.make_pmf())
    wei_rematch = MakeBinomialMix2(wei.make_pmf())
    rhode_rematch.prob_greater(wei_rematch), rhode_rematch.prob_less(wei_rematch)

    # **Exercise:** Run this analysis again with a different prior and see how much effect it has on the results.

    # ## Distributions of sums and differences
    #
    # Suppose we want to know the total number of targets the two contestants will hit in a rematch.  There are two ways we might compute the distribution of this sum:
    #
    # 1. Sampling: We can draw samples from the distributions and add them up.
    #
    # 2. Enumeration: We can enumerate all possible pairs of values.
    #
    # I'll start with sampling:

    iters = 1000
    pmf = Pmf()
    for _ in range(iters):
        k = rhode_rematch.random() + wei_rematch.random()
        pmf[k] += 1
    pmf.normalize()
    thinkplot.plot_hist_bar(pmf)

    # Or we could use `Sample` and NumPy:

    ks = rhode_rematch.sample(iters) + wei_rematch.sample(iters)
    pmf = Pmf(ks)
    thinkplot.plot_hist_bar(pmf)

    # Alternatively, we could compute the distribution of the sum by enumeration:

    def AddPmfs(pmf1, pmf2):
        pmf = Pmf()
        for v1, p1 in pmf1.items():
            for v2, p2 in pmf2.items():
                pmf[v1 + v2] += p1 * p2
        return pmf

    # Here's how it's used:

    pmf = AddPmfs(rhode_rematch, wei_rematch)
    thinkplot.plot_pdf_line(pmf)

    # The `Pmf` class provides a `+` operator that does the same thing.

    pmf = rhode_rematch + wei_rematch
    thinkplot.plot_pdf_line(pmf)

    # **Exercise:**  The Pmf class also provides the `-` operator, which computes the distribution of the difference in values from two distributions.  Use the distributions from the previous section to compute the distribution of the differential between Rhode and Wei in a rematch.  On average, how many clays should we expect Rhode to win by?  What is the probability that Rhode wins by 10 or more?

    # Solution

    pmf = rhode_rematch - wei_rematch
    thinkplot.plot_pdf_line(pmf)

    # Solution

    # On average, we expect Rhode to win by about 1 clay.

    pmf.mean(), pmf.median(), pmf.mode()

    # Solution

    # But there is, according to this model, a 2% chance that she could win by 10.

    p_win_by_10 = sum([p for (x, p) in pmf.items() if x >= 10])
    logging.info("%r", f"p_win_by_10 = {p_win_by_10}")

    # ## Distribution of maximum
    #
    # Suppose Kim Rhode continues to compete in six more Olympics.  What should we expect her best result to be?
    #
    # Once again, there are two ways we can compute the distribution of the maximum:
    #
    # 1. Sampling.
    #
    # 2. Analysis of the CDF.
    #
    # Here's a simple version by sampling:

    iters = 1000
    pmf = Pmf()
    for _ in range(iters):
        ks = rhode_rematch.sample(6)
        pmf[max(ks)] += 1
    pmf.normalize()
    thinkplot.plot_hist_bar(pmf)

    # And here's a version using NumPy.  I'll generate an array with 6 rows and 10 columns:

    iters = 1000
    ks = rhode_rematch.sample((6, iters))
    logging.info("%r", f"ks = {ks}")

    # Compute the maximum in each column:

    maxes = np.max(ks, axis=0)
    logging.info("%r", f"maxes[:10] = {maxes[:10]}")

    # And then plot the distribution of maximums:

    pmf = Pmf(maxes)
    thinkplot.plot_hist_bar(pmf)

    # Or we can figure it out analytically.  If the maximum is less-than-or-equal-to some value `k`, all 6 random selections must be less-than-or-equal-to `k`, so:
    #
    # $ CDF_{max}(x) = CDF(x)^6 $
    #
    # `Pmf` provides a method that computes and returns this `Cdf`, so we can compute the distribution of the maximum like this:

    pmf = rhode_rematch.max(6).make_pmf()
    thinkplot.plot_hist_bar(pmf)

    # **Exercise:**  Here's how Pmf.Max works:
    #
    #     def Max(self, k):
    #         """Computes the CDF of the maximum of k selections from this dist.
    #
    #         k: int
    #
    #         returns: new Cdf
    #         """
    #         cdf = self.MakeCdf()
    #         cdf.ps **= k
    #         return cdf
    #
    # Write a function that takes a Pmf and an integer `n` and returns a Pmf that represents the distribution of the minimum of `k` values drawn from the given Pmf.  Use your function to compute the distribution of the minimum score Kim Rhode would be expected to shoot in six competitions.

    def Min(pmf, k):
        cdf = pmf.make_cdf()
        cdf.ps = 1 - (1 - cdf.ps) ** k
        return cdf

    pmf = Min(rhode_rematch, 6).make_pmf()
    thinkplot.plot_hist_bar(pmf)
