"""
Think Bayes
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import math
import numpy as np
import pandas as pd

from thinkbayes import Pmf, Cdf, Suite, Joint
from thinkbayes import thinkplot


def test_birthday_problem():
    # ### The August birthday problem
    #
    # This article:
    #
    # [Attention Deficit–Hyperactivity Disorder and Month of School Enrollment](https://www.nejm.org/doi/10.1056/NEJMoa1806828)
    #
    # Finds:
    #
    # >The rate of claims-based ADHD diagnosis among children in states with a September 1 cutoff was 85.1 per 10,000 children (309 cases among 36,319 children; 95% confidence interval [CI], 75.6 to 94.2) among those born in August and 63.6 per 10,000 children (225 cases among 35,353 children; 95% CI, 55.4 to 71.9) among those born in September, an absolute difference of 21.5 per 10,000 children (95% CI, 8.8 to 34.0); the corresponding difference in states without the September 1 cutoff was 8.9 per 10,000 children (95% CI, −14.9 to 20.8). The rate of ADHD treatment was 52.9 per 10,000 children (192 of 36,319 children; 95% CI, 45.4 to 60.3) among those born in August and 40.4 per 10,000 children (143 of 35,353 children; 95% CI, 33.8 to 47.1) among those born in September, an absolute difference of 12.5 per 10,000 children (95% CI, 2.43 to 22.4). These differences were not observed for other month-to-month comparisons, nor were they observed in states with non-September cutoff dates for starting kindergarten. In addition, in states with a September 1 cutoff, no significant differences between August-born and September-born children were observed in rates of asthma, diabetes, or obesity.
    #
    # It includes this figure:
    #
    # ![](https://www.nejm.org/na101/home/literatum/publisher/mms/journals/content/nejm/2018/nejm_2018.379.issue-22/nejmoa1806828/20181123/images/img_xlarge/nejmoa1806828_f1.jpeg)
    #
    # However, there is an error in this figure, confirmed by personal correspondence:
    #
    # >The May and June [diagnoses] are reversed. May should be 317 (not 287) and June should be 287 (not 317).
    #
    #
    # Based on this corrected data, what can we say about the probability of diagnosis as a function of birth month?
    #
    # What can we say about the rate of misdiagnosis?

    # Here's the data from the table.

    totals = np.array(
        [32690, 31238, 34405, 34565, 34977, 34415, 36577, 36319, 35353, 34405, 31285, 31617]
    )

    diagnosed = np.array([265, 280, 307, 312, 317, 287, 320, 309, 225, 240, 232, 243])

    # I'll roll the data so September comes first.

    totals = np.roll(totals, -8)
    diagnosed = np.roll(diagnosed, -8)

    # Here are the diagnosis rates, which we can check against the rates in the table.

    rates = diagnosed / totals * 10000
    np.round(rates, 1)

    # Here's what the rates look like as a function of months after the September cutoff.

    xs = np.arange(12)
    thinkplot.plot(xs, rates)
    thinkplot.decorate(xlabel="Months after cutoff", ylabel="Diagnosis rate per 10,000")

    # For the first 9 months, from September to May, we see what we would expect if at least some of the excess diagnoses are due to behavioral differences due to age.  For each month of difference in age, we see an increase in the number of diagnoses.
    #
    # This pattern breaks down for the last three months, June, July, and August.  This might be explained by random variation, but it also might be due to parental manipulation; if some parents hold back students born near the deadline, the observations for these month would include a mixture of children who are relatively old for their grade, and therefore less likely to be diagnosed.
    #
    # We could test this hypothesis by checking the actual ages of these students when they started school, rather than just looking at their months of birth.
    #
    # I'll use a beta distribution to compute the posterior credible interval for each of these rates.

    import scipy.stats

    pcount = 1
    res = []
    for (x, d, t) in zip(xs, diagnosed, totals):
        a = d + pcount
        b = t - d + pcount
        ci = scipy.stats.beta(a, b).ppf([0.025, 0.975])
        res.append(ci * 10000)

    # By transposing the results, we can get them into two arrays for plotting.

    low, high = np.transpose(res)

    low

    high

    # Here's what the plot looks like with error bars.

    import matplotlib.pyplot as plt

    def errorbar(xs, low, high, **options):
        for x, l, h in zip(xs, low, high):
            plt.vlines(x, l, h, **options)

    errorbar(xs, low, high, color="gray", alpha=0.7)
    thinkplot.plot(xs, rates)
    thinkplot.decorate(xlabel="Months after cutoff", ylabel="Diagnosis rate per 10,000")

    # It seems like the lower rates in the last 3 months are unlikely to be due to random variation, so it might be good to investigate the effect of "red shirting".
    #
    # But for now I will proceed with a linear logistic model.  The following table shows log odds of diagnosis for each month, which I will use to lay out a grid for parameter estimation.

    from scipy.special import expit, logit

    for (x, d, t) in zip(xs, diagnosed, totals):
        print(x, logit(d / t))

    # Here's a Suite that estimates the parameters of a logistic regression model, `b0` and `b1`.

    class August(Suite, Joint):
        def Likelihood(self, data, hypo):
            x, d, t = data
            b0, b1 = hypo

            p = expit(b0 + b1 * x)
            like = scipy.stats.binom.pmf(d, t, p)

            return like

    # The prior distributions are uniform over a grid that covers the most likely values.

    from itertools import product

    b0 = np.linspace(-4.75, -5.1, 101)
    b1 = np.linspace(-0.05, 0.05, 101)
    hypos = product(b0, b1)

    suite = August(hypos)

    # Here's the update.

    for data in zip(xs, diagnosed, totals):
        suite.Update(data)

    # Here's the posterior marginal distribution for `b0`.

    pmf0 = suite.Marginal(0)
    b0 = pmf0.Mean()
    print(b0)
    thinkplot.Pdf(pmf0)

    thinkplot.decorate(
        title="Posterior marginal distribution",
        xlabel="Intercept log odds (b0)",
        ylabel="Pdf",
    )

    # And the posterior marginal distribution for `b1`.

    pmf1 = suite.Marginal(1)
    b1 = pmf1.Mean()
    print(b1)
    thinkplot.Pdf(pmf1)

    thinkplot.decorate(
        title="Posterior marginal distribution", xlabel="Slope log odds (b0)", ylabel="Pdf"
    )

    # Let's see what the posterior regression lines look like, superimposed on the data.

    for i in range(100):
        b0, b1 = suite.Random()
        ys = expit(b0 + b1 * xs) * 10000
        thinkplot.plot(xs, ys, color="green", alpha=0.01)

    errorbar(xs, low, high, color="gray", alpha=0.7)
    thinkplot.plot(xs, rates)

    thinkplot.decorate(xlabel="Months after cutoff", ylabel="Diagnosis rate per 10,000")

    # Most of these regression lines fall within the credible intervals of the observed rates, so in that sense it seems like this model is not ruled out by the data.
    #
    # But it is clear that the lower rates in the last 3 months bring down the estimated slope, so we should probably treat the estimated effect size as a lower bound.
    #
    # To express the results more clearly, we can look at the posterior predictive distribution for the difference between a child born in September and one born in August:

    def posterior_predictive(x):
        pmf = Pmf()

        for (b0, b1), p in suite.Items():
            base = expit(b0 + b1 * x) * 10000
            pmf[base] += p

        return pmf

    # Here are posterior predictive CDFs for diagnosis rates.

    pmf0 = posterior_predictive(0)
    thinkplot.Cdf(pmf0.MakeCdf(), label="September")

    pmf1 = posterior_predictive(11)
    thinkplot.Cdf(pmf1.MakeCdf(), label="August")

    thinkplot.decorate(
        title="Posterior predictive distribution",
        xlabel="Diagnosis rate per 10,000",
        ylabel="CDF",
    )

    pmf0.Mean()

    # And we can compute the posterior predictive distribution for the difference.

    def posterior_predictive_diff():
        pmf = Pmf()

        for (b0, b1), p in suite.Items():
            p0 = expit(b0) * 10000
            p1 = expit(b0 + b1 * 11) * 10000
            diff = p1 - p0
            pmf[diff] += p

        return pmf

    pmf_diff = posterior_predictive_diff()
    thinkplot.Cdf(pmf_diff.MakeCdf())

    thinkplot.decorate(
        title="Posterior predictive distribution",
        xlabel="11 month increase in diagnosis rate per 10,000",
        ylabel="CDF",
    )

    # To summarize, we can compute the mean and 95% credible interval for this difference.

    pmf_diff.Mean()

    #

    pmf_diff.CredibleInterval(95)

    # A difference of 21 diagnoses, on a base rate of 71 diagnoses, is an increase of 30% (18%, 42%)

    pmf_diff.Mean() / pmf0.Mean()

    pmf_diff.CredibleInterval(95) / pmf0.Mean()
