"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf, Suite

import pandas as pd
import numpy as np

from thinkbayes import thinkplot


def test_interpreting_medical_tests():
    # ## Interpreting medical tests
    #
    # Suppose you are a doctor treating a 40-year old female patient.  After she gets a routine screening mammogram, the result comes back positive (defined below).
    #
    # The patient asks whether this result indicates that she has breast cancer.  You interpret this question as, "What is the probability that this patient has breast cancer, given a positive test result?"
    #
    # How would you respond?
    #
    # The following background information from the Breast Cancer Screening Consortium (BCSC) might help:
    #
    # [Cancer Rate (per 1,000 examinations) and Cancer Detection Rate (per 1,000 examinations) for 1,838,372 Screening Mammography Examinations from 2004 to 2008 by Age -- based on BCSC data through 2009](http://www.bcsc-research.org/statistics/performance/screening/2009/rate_age.html).
    #
    # [Performance Measures for 1,838,372 Screening Mammography Examinations1 from 2004 to 2008 by Age -- based on BCSC data through 2009](http://www.bcsc-research.org/statistics/performance/screening/2009/perf_age.html).

    class BayesTable(pd.DataFrame):
        def __init__(self, hypo, prior=1, **options):
            columns = ["prior", "likelihood", "unnorm", "posterior"]
            super().__init__(index=hypo, columns=columns, **options)
            self.prior = prior

        def mult(self):
            self.unnorm = self.prior * self.likelihood

        def norm(self):
            nc = np.sum(self.unnorm)
            self.posterior = self.unnorm / nc
            return nc

        def update(self):
            self.mult()
            return self.norm()

        def reset(self):
            return BayesTable(self.hypo, self.posterior)

    # ### Assumptions and interpretation
    #
    # According to [the first table](http://www.bcsc-research.org/statistics/performance/screening/2009/rate_age.html), the cancer rate per 1000 examinations is 2.65 for women age 40-44.  The notes explain that this rate is based on "the number of examinations with a tissue diagnosis of ductal carcinoma in situ or invasive cancer within 1 year following the examination and before the next screening mammography examination", so it would be more precise to say that it is the rate of diagnosis within a year of the examination, not the rate of actual cancers.
    #
    # Since untreated invasive breast cancer is likely to become symptomatic, we expect a large fraction of cancers to be diagnosed eventually.  But there might be a long delay between developing a cancer and diagnosis, and a patient might die of another cause before diagnosis.  So we should consider this rate as a lower bound on the probability that a patient has cancer at the time of the examination.
    #
    # According to [the second table](http://www.bcsc-research.org/statistics/performance/screening/2009/perf_age.html), the sensitivity of the test for women in this age group is 73.4%; the specificity is 87.7%.  From these, we can get the conditional probabilities:
    #
    # ```
    # P(positive test | cancer) = sensitivity
    # P(positive test | no cancer) = (1 - specificity)
    # ```
    #
    # Now we can use a Bayes table to compute the probability we are interested in, `P(cancer | positive test)`

    base_rate = 2.65 / 1000
    hypo = ["cancer", "no cancer"]
    prior = [base_rate, 1 - base_rate]
    table = BayesTable(hypo, prior)

    sensitivity = 0.734
    specificity = 0.877
    table.likelihood = [sensitivity, 1 - specificity]
    table

    likelihood_ratio = table.likelihood["cancer"] / table.likelihood["no cancer"]

    table.update()
    table

    table.posterior["cancer"] * 100

    # So there is a 1.56% chance that this patient has cancer, given that the initial screening mammogram was positive.
    #
    # This result is called the positive predictive value (PPV) of the test, which we could have read from [the second table](http://www.bcsc-research.org/statistics/performance/screening/2009/perf_age.html)

    # This data was the basis, in 2009, for the recommendation of the US Preventive Services Task Force,

    def compute_ppv(base_rate, sensitivity, specificity):
        pmf = Pmf()
        pmf["cancer"] = base_rate * sensitivity
        pmf["no cancer"] = (1 - base_rate) * (1 - specificity)
        pmf.Normalize()
        return pmf

    pmf = compute_ppv(base_rate, sensitivity, specificity)

    ages = [40, 50, 60, 70, 80]
    rates = pd.Series([2.65, 4.28, 5.70, 6.76, 8.51], index=ages)

    for age, rate in rates.items():
        pmf = compute_ppv(rate, sensitivity, specificity)
        print(age, pmf["cancer"])
