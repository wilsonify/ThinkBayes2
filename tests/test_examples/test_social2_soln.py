"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy as np
from thinkbayes import Suite, Beta
from thinkbayes import thinkplot


def test_social_desirability_problem():
    # ## The social desirability problem
    #
    # Whenever you survey people about sensitive issues, you have to deal with [social desirability bias](https://en.wikipedia.org/wiki/Social_desirability_bias), which is the tendency of people to shade their answers in the direction they think shows them in the most positive light.
    #
    # One of the ways to improve the quality of the results is to collect responses in indirect ways.  For example, [here's a clever way one research group estimated the prevalence of atheists](https://fivethirtyeight.com/features/way-more-americans-may-be-atheists-than-we-thought/).
    #
    # Another way is [randomized response](https://en.wikipedia.org/wiki/Randomized_response), as described in [this presentation](http://www.soz.unibe.ch/ueber_uns/personen/jann/presentations_by_ben_jann/e131361/e131381/rrt_online07_kassel08_ger.pdf) or [this video](https://www.youtube.com/watch?v=nwJ0qY_rP0A).
    #
    # As an example, suppose you ask 100 people to flip a coin and:
    #
    # * If they get heads, they honestly answer the question "Do you believe in God?"
    #
    # * If they get tails, they flip a second coin and report YES for heads, tails for NO.
    #
    # Assume that you cannot observe whether they flip one coin or two.
    # And suppose you get 55 YESes and 45 NOs.
    #
    # 1. Estimate the prevalence of believers in the surveyed population (by which, as always, I mean compute a posterior distribution).
    #
    # 2. How efficient is this method?  That is, how does the width of the posterior distribution compare to the distribution you would get if 100 people answered the question honestly?

    # Solution

    class Social(Suite):
        def likelihood(self, data, hypo):
            """
            data: outcome of unreliable measurement, either 'YES' or 'NO'
            hypo: actual proportion of the thing we're measuring
            """
            p = hypo
            p_yes = 0.25 + p / 2
            if data == "YES":
                return p_yes
            else:
                return 1 - p_yes

    # Solution

    prior = np.linspace(0, 1, 101)
    suite = Social(prior)

    thinkplot.plot_pdf_line(suite, label="Prior")
    thinkplot.decorate(xlabel="Fraction of the population", ylabel="PDF")

    # Solution

    for i in range(55):
        suite.update("YES")

    for i in range(45):
        suite.update("NO")

    # Solution

    thinkplot.plot_pdf_line(suite, label="Posterior")
    thinkplot.decorate(xlabel="Fraction of the population", ylabel="PDF")

    # Solution

    suite.mean(), suite.MAP()

    # Solution

    # For comparison, what would we think if we had been able
    # to survey 100 people directly?

    beta = Beta(1, 1)
    beta.update((60, 40))
    thinkplot.plot_pdf_line(beta.make_pmf(), label="Direct", color="gray")

    thinkplot.plot_pdf_line(suite, label="Randomized")
    thinkplot.decorate(xlabel="Fraction of the population", ylabel="PDF")

    # Solution

    # To see how efficient this method is, we can divide the sample size for
    # the direct method by a factor.  It looks like we lose a factor of about 4.

    factor = 4
    beta = Beta(1, 1)
    beta.update((60 / factor, 40 / factor))
    thinkplot.plot_pdf_line(beta.make_pmf(), label="Direct", color="gray")

    thinkplot.plot_pdf_line(suite, label="Randomized")
    thinkplot.decorate(xlabel="Fraction of the population", ylabel="PDF")

    # Solution

    # So the effective sample size is about 25.
