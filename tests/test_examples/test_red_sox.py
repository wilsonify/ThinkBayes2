"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy as np
from thinkbayes import Pmf, Beta
import thinkplot


def test_beta():
    beta = Beta(5, 5)
    prior = beta.MakePmf()
    beta.Update((15, 0))
    posterior = beta.MakePmf()
    posterior.Mean()
    posterior.MAP()
    posterior.CredibleInterval()
    x = posterior.Random()
    np.sum(np.random.random(7) < x)
    def simulate(k, dist):
        x = dist.Random()
        return np.sum(np.random.random(k) <= x)

    simulate(7, posterior)
    sample = [simulate(7, posterior) for i in range(100000)]
    np.mean(np.array(sample) >= 4)
