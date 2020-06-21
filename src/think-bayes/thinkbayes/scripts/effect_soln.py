"""This file contains code used in "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
from random import random

import thinkbayes
from thinkbayes import thinkplot
from thinkbayes.scripts.variability import update_suite5, Height, find_prior_ranges, summarize, read_heights


def run_estimate(update_func, num_points=31, median_flag=False):
    """Runs the whole analysis.

    update_func: which of the update functions to use
    num_points: number of points in the Suite (in each dimension)
    """
    d = read_heights(nrows=None)
    labels = {1: "male", 2: "female"}

    suites = {}
    for key, xs in d.items():
        label = labels[key]
        print(label, len(xs))
        summarize(xs)

        xs = thinkbayes.Jitter(xs, 1.3)

        mus, sigmas = find_prior_ranges(xs, num_points, median_flag=median_flag)
        suite = Height(mus, sigmas, label)
        suites[label] = suite
        update_func(suite, xs)
        print("MAP", suite.MaximumLikelihood())

    suite1 = suites["male"]
    suite2 = suites["female"]

    mu1 = suite1.Marginal(0)
    sigma1 = suite1.Marginal(1)

    mu2 = suite2.Marginal(0)
    sigma2 = suite2.Marginal(1)

    diff = mu1 - mu2
    sigma = (sigma1 + sigma2) / 2

    pmf_d = diff / sigma

    thinkplot.plot_cdf_line(pmf_d.MakeCdf())
    thinkplot.show_plot(xlabel="# stddev between means", ylabel="PMF")


def main():
    random.seed(17)

    func = update_suite5
    median_flag = func == update_suite5
    run_estimate(func, median_flag=median_flag)


if __name__ == "__main__":
    main()
