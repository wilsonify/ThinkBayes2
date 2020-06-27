"""
# # Think Bayes
#
# Copyright 2018 Allen B. Downey
#
# MIT License: https://opensource.org/licenses/MIT
"""
import logging
import os

import numpy as np
import pandas as pd
from thinkbayes import Suite, Joint
from thinkbayes import thinkplot
from thinkbayes.thinkplot import POSTERIOR_MARGINAL_LABEL

TESTDIR = os.path.abspath(os.path.dirname(__file__))
DATADIR = os.path.join(TESTDIR, "data")


def test_shuttle():
    """
    The Space Shuttle problem
    Here's a problem from [Bayesian Methods for Hackers](http://nbviewer.jupyter.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter2_MorePyMC/Ch2_MorePyMC_PyMC2.ipynb)
    >On January 28, 1986, the twenty-fifth flight of the U.S. space shuttle program ended in disaster
    when one of the rocket boosters of the Shuttle Challenger exploded shortly after lift-off,
    killing all seven crew members.
    The presidential commission on the accident concluded that it was caused by the failure of an O-ring
    in a field joint on the rocket booster, and that this failure was due to a faulty design that
    made the O-ring unacceptably sensitive to a number of factors including outside temperature.
    Of the previous 24 flights, data were available on failures of O-rings on 23,
    (one was lost at sea), and these data were discussed on the evening preceding the Challenger launch,
    but unfortunately only the data corresponding to the 7 flights on which there was a damage incident
    were considered important and these were thought to show no obvious trend.
    The data are shown below (see [1](https://amstat.tandfonline.com/doi/abs/10.1080/01621459.1989.10478858)):
    !wget https://raw.githubusercontent.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/master/Chapter2_MorePyMC/data/challenger_data.csv
    """
    columns = ["Date", "Temperature", "Incident"]
    df = pd.read_csv(os.path.join(DATADIR, "challenger_data.csv"), parse_dates=[0])
    df.drop(labels=[3, 24], inplace=True)
    logging.info("%r", f"df.shape = {df.shape}")

    df["Incident"] = df["Damage Incident"].astype(float)
    logging.info("%r", f"df.shape = {df.shape}")

    import matplotlib.pyplot as plt

    plt.scatter(df.Temperature, df.Incident, s=75, color="k", alpha=0.5)
    plt.yticks([0, 1])
    plt.ylabel("Damage Incident?")
    plt.xlabel("Outside temperature (Fahrenheit)")
    plt.title("Defects of the Space Shuttle O-Rings vs temperature")

    # ### Grid algorithm
    #
    # We can solve the problem first using a grid algorithm, with parameters `b0` and `b1`, and
    #
    # $\mathrm{logit}(p) = b0 + b1 * T$
    #
    # and each datum being a temperature `T` and a boolean outcome `fail`, which is true is there was damage and false otherwise.
    #
    # Hint: the `expit` function from `scipy.special` computes the inverse of the `logit` function.

    class Logistic(Suite, Joint):
        def likelihood(self, data, hypo):
            """

            data: T, fail
            hypo: b0, b1
            """
            return 1

    # Solution

    from scipy.special import expit

    class Logistic(Suite, Joint):
        def likelihood(self, data, hypo):
            """

            data: T, fail
            hypo: b0, b1
            """
            temp, fail = data
            b0, b1 = hypo

            log_odds = b0 + b1 * temp
            p_fail = expit(log_odds)
            if fail == 1:
                return p_fail
            elif fail == 0:
                return 1 - p_fail
            else:
                # NaN
                return 1

    b0 = np.linspace(0, 50, 101)

    b1 = np.linspace(-1, 1, 101)

    from itertools import product

    hypos = product(b0, b1)

    suite = Logistic(hypos)

    for data in zip(df.Temperature, df.Incident):
        print(data)
        suite.update(data)

    thinkplot.plot_pdf_line(suite.marginal(0))
    thinkplot.decorate(
        xlabel="Intercept", ylabel="PMF", title=POSTERIOR_MARGINAL_LABEL
    )

    thinkplot.plot_pdf_line(suite.marginal(1))
    thinkplot.decorate(
        xlabel="Log odds ratio", ylabel="PMF", title=POSTERIOR_MARGINAL_LABEL
    )

    # According to the posterior distribution, what was the probability of damage when the shuttle launched at 31 degF?

    # Solution

    T = 31
    total = 0

    for hypo, p in suite.items():
        b0, b1 = hypo
        log_odds = b0 + b1 * T
        p_fail = expit(log_odds)
        total += p * p_fail

    logging.info("%r", f"total = {total}")

    # Solution

    pred = suite.copy()
    pred.update((31, True))

    # ### MCMC
    #
    # Implement this model using MCMC.  As a starting place, you can use this example from [the PyMC3 docs](https://docs.pymc.io/notebooks/GLM-logistic.html#The-model).
    #
    # As a challege, try writing the model more explicitly, rather than using the GLM module.

    from warnings import simplefilter

    simplefilter("ignore", FutureWarning)

    import pymc3 as pm

    # Solution

    with pm.Model() as model:
        pm.glm.GLM.from_formula(
            "Incident ~ Temperature", df, family=pm.glm.families.Binomial()
        )

        start = pm.find_MAP()
        trace = pm.sample(1000, start=start, tune=1000)

    pm.traceplot(trace)

    # Solution

    with pm.Model() as model:
        pm.glm.GLM.from_formula(
            "Incident ~ Temperature", df, family=pm.glm.families.Binomial()
        )

        trace = pm.sample(1000, tune=1000)

    # The posterior distributions for these parameters should be similar to what we got with the grid algorithm.
