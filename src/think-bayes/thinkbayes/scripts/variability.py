"""This file contains code used in "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import math
import random

import matplotlib.pyplot as pyplot
import numpy
import scipy
import thinkbayes
from scipy import stats
from thinkbayes import thinkplot
from thinkbayes.scripts import brfss

NUM_SIGMAS = 1


class Height(thinkbayes.Suite, thinkbayes.Joint):
    """Hypotheses about parameters of the distribution of height."""

    def __init__(self, mus, sigmas, label=None):
        """Makes a prior distribution for mu and sigma based on a sample.

        mus: sequence of possible mus
        sigmas: sequence of possible sigmas
        label: string label for the Suite
        """
        pairs = [(mu, sigma) for mu in mus for sigma in sigmas]

        thinkbayes.Suite.__init__(self, pairs, label=label)

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        Args:
            hypo: tuple of hypothetical mu and sigma
            data: float sample

        Returns:
            likelihood of the sample given mu and sigma
        """
        x = data
        mu, sigma = hypo
        like = scipy.stats.norm.pdf(x, mu, sigma)
        return like

    def log_likelihood(self, data, hypo):
        """Computes the log likelihood of the data under the hypothesis.

        Args:
            data: a list of values
            hypo: tuple of hypothetical mu and sigma

        Returns:
            log likelihood of the sample given mu and sigma (unnormalized)
        """
        x = data
        mu, sigma = hypo
        loglike = eval_normal_log_pdf(x, mu, sigma)
        return loglike

    def log_update_set_fast(self, data):
        """Updates the suite using a faster implementation.

        Computes the sum of the log likelihoods directly.

        Args:
            data: sequence of values
        """
        xs = tuple(data)
        n = len(xs)

        for hypo in self.values():
            mu, sigma = hypo
            total = summation(xs, mu)
            loglike = -n * math.log(sigma) - total / 2 / sigma ** 2
            self.incr(hypo, loglike)

    def log_update_set_mean_var(self, data):
        """Updates the suite using ABC and mean/var.

        Args:
            data: sequence of values
        """
        xs = data
        n = len(xs)

        m = numpy.mean(xs)
        s = numpy.std(xs)

        self.log_update_set_abc(n, m, s)

    def log_update_set_median_ipr(self, data):
        """Updates the suite using ABC and median/iqr.

        Args:
            data: sequence of values
        """
        xs = data
        n = len(xs)

        # compute summary stats
        median, s = median_s(xs, num_sigmas=NUM_SIGMAS)
        print("median, s", median, s)

        self.log_update_set_abc(n, median, s)

    def log_update_set_abc(self, n, m, s):
        """Updates the suite using ABC.

        n: sample size
        m: estimated central tendency
        s: estimated spread
        """
        for hypo in sorted(self.values()):
            mu, sigma = hypo

            # compute log likelihood of m, given hypo
            stderr_m = sigma / math.sqrt(n)
            loglike = eval_normal_log_pdf(m, mu, stderr_m)

            # compute log likelihood of s, given hypo
            stderr_s = sigma / math.sqrt(2 * (n - 1))
            loglike += eval_normal_log_pdf(s, sigma, stderr_s)

            self.incr(hypo, loglike)


def eval_normal_log_pdf(x, mu, sigma):
    """Computes the log PDF of x given mu and sigma.

    x: float values
    mu, sigma: paramemters of Normal

    returns: float log-likelihood
    """
    return scipy.stats.norm.logpdf(x, mu, sigma)


def find_prior_ranges(xs, num_points, num_stderrs=3.0, median_flag=False):
    """Find ranges for mu and sigma with non-negligible likelihood.

    xs: sample
    num_points: number of values in each dimension
    num_stderrs: number of standard errors to include on either side
    
    Returns: sequence of mus, sequence of sigmas    
    """

    def make_range(estimate, stderr):
        """Makes a linear range around the estimate.

        estimate: central value
        stderr: standard error of the estimate

        returns: numpy array of float
        """
        spread = stderr * num_stderrs
        array = numpy.linspace(estimate - spread, estimate + spread, num_points)
        return array

    # estimate mean and stddev of xs
    n = len(xs)
    if median_flag:
        m, s = median_s(xs, num_sigmas=NUM_SIGMAS)
    else:
        m = numpy.mean(xs)
        s = numpy.std(xs)

    print("classical estimators", m, s)

    # compute ranges for m and s
    stderr_m = s / math.sqrt(n)
    mus = make_range(m, stderr_m)

    stderr_s = s / math.sqrt(2 * (n - 1))
    sigmas = make_range(s, stderr_s)

    return mus, sigmas


def summation(xs, mu, cache=None):
    """Computes the sum of (x-mu)**2 for x in t.

    Caches previous results.

    xs: tuple of values
    mu: hypothetical mean
    cache: cache of previous results
    """
    if cache is None:
        cache = {}
    try:
        return cache[xs, mu]
    except KeyError:
        ds = [(x - mu) ** 2 for x in xs]
        total = sum(ds)
        cache[xs, mu] = total
        return total


def coef_variation(suite):
    """Computes the distribution of CV.

    suite: Pmf that maps (x, y) to z

    Returns: Pmf object for CV.
    """
    pmf = thinkbayes.Pmf()
    for (m, s), p in suite.items():
        pmf.incr(s / m, p)
    return pmf


def plot_cdfs(d, labels):
    """Plot CDFs for each sequence in a dictionary.

    Jitters the data and subtracts away the mean.

    d: map from key to sequence of values
    labels: map from key to string label
    """
    thinkplot.clear_figure()
    for key, xs in d.items():
        mu = thinkbayes.mean(xs)
        xs = thinkbayes.jitter(xs, 1.3)
        xs = [x - mu for x in xs]
        cdf = thinkbayes.make_cdf_from_list(xs)
        thinkplot.plot_cdf_line(cdf, label=labels[key])
    thinkplot.show_plot()


def plot_posterior(suite, pcolor=False, contour=True):
    """Makes a contour plot.
    
    suite: Suite that maps (mu, sigma) to probability
    """
    thinkplot.clear_figure()
    thinkplot.contour_plot(suite.GetDict(), pcolor_bool=pcolor, contour_bool=contour)

    thinkplot.save_plot(
        root="variability_posterior_%s" % suite.label,
        title="Posterior joint distribution",
        xlabel="Mean height (cm)",
        ylabel="Stddev (cm)",
    )


def plot_coef_variation(suites):
    """Plot the posterior distributions for CV.

    suites: map from label to Pmf of CVs.
    """
    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)

    pmfs = {}
    for label, suite in suites.items():
        pmf = coef_variation(suite)
        print("CV posterior mean", pmf.mean())
        cdf = thinkbayes.make_cdf_from_pmf(pmf, label)
        thinkplot.plot_cdf_line(cdf)

        pmfs[label] = pmf

    thinkplot.save_plot(
        root="variability_cv", xlabel="Coefficient of variation", ylabel="Probability"
    )

    print("female bigger", thinkbayes.pmf_prob_greater(pmfs["female"], pmfs["male"]))
    print("male bigger", thinkbayes.pmf_prob_greater(pmfs["male"], pmfs["female"]))


def plot_outliers(samples):
    """Make CDFs showing the distribution of outliers."""
    cdfs = []
    for label, sample in samples.items():
        outliers = [x for x in sample if x < 150]

        cdf = thinkbayes.make_cdf_from_list(outliers, label)
        cdfs.append(cdf)

    thinkplot.clear_figure()
    thinkplot.plot_cdfs(cdfs)
    thinkplot.save_plot(
        root="variability_cdfs",
        title="CDF of height",
        xlabel="Reported height (cm)",
        ylabel="CDF",
    )


def plot_marginals(suite):
    """Plots marginal distributions from a joint distribution.

    suite: joint distribution of mu and sigma.
    """
    thinkplot.clear_figure()

    pyplot.subplot(1, 2, 1)
    pmf_m = suite.marginal(0)
    cdf_m = thinkbayes.make_cdf_from_pmf(pmf_m)
    thinkplot.plot_cdf_line(cdf_m)

    pyplot.subplot(1, 2, 2)
    pmf_s = suite.marginal(1)
    cdf_s = thinkbayes.make_cdf_from_pmf(pmf_s)
    thinkplot.plot_cdf_line(cdf_s)

    thinkplot.show_plot()


def read_heights(nrows=None):
    """Read the BRFSS dataset, extract the heights and pickle them.

    nrows: number of rows to read
    """
    resp = brfss.read_brfss(nrows=nrows).dropna(subset=["sex", "htm3"])
    groups = resp.groupby("sex")

    d = {}
    for name, group in groups:
        d[name] = group.htm3.values

    return d


def update_suite1(suite, xs):
    """Computes the posterior distibution of mu and sigma.

    Computes untransformed likelihoods.

    suite: Suite that maps from (mu, sigma) to prob
    xs: sequence
    """
    suite.update_set(xs)


def update_suite2(suite, xs):
    """Computes the posterior distibution of mu and sigma.

    Computes log likelihoods.

    suite: Suite that maps from (mu, sigma) to prob
    xs: sequence
    """
    suite.log()
    suite.log_update_set(xs)
    suite.exp()
    suite.normalize()


def update_suite3(suite, xs):
    """Computes the posterior distibution of mu and sigma.

    Computes log likelihoods efficiently.

    suite: Suite that maps from (mu, sigma) to prob
    t: sequence
    """
    suite.log()
    suite.log_update_set_fast(xs)
    suite.exp()
    suite.normalize()


def update_suite4(suite, xs):
    """Computes the posterior distibution of mu and sigma.

    Computes log likelihoods efficiently.

    suite: Suite that maps from (mu, sigma) to prob
    t: sequence
    """
    suite.log()
    suite.log_update_set_mean_var(xs)
    suite.exp()
    suite.normalize()


def update_suite5(suite, xs):
    """Computes the posterior distibution of mu and sigma.

    Computes log likelihoods efficiently.

    suite: Suite that maps from (mu, sigma) to prob
    t: sequence
    """
    suite.log()
    suite.log_update_set_median_ipr(xs)
    suite.exp()
    suite.normalize()


def median_ipr(xs, p):
    """Computes the median and interpercentile range.

    xs: sequence of values
    p: range (0-1), 0.5 yields the interquartile range

    returns: tuple of float (median, IPR)
    """
    cdf = thinkbayes.make_cdf_from_list(xs)
    median = cdf.percentile(50)

    alpha = (1 - p) / 2
    ipr = cdf.value(1 - alpha) - cdf.value(alpha)
    return median, ipr


def median_s(xs, num_sigmas):
    """Computes the median and an estimate of sigma.

    Based on an interpercentile range (IPR).

    factor: number of standard deviations spanned by the IPR
    """
    half_p = thinkbayes.standard_normal_cdf(num_sigmas) - 0.5
    median, ipr = median_ipr(xs, half_p * 2)
    s = ipr / 2 / num_sigmas

    return median, s


def summarize(xs):
    """Prints summary statistics from a sequence of values.

    xs: sequence of values
    """
    # print smallest and largest
    xs.sort()
    print("smallest", xs[:10])
    print("largest", xs[-10:])

    # print median and interquartile range
    cdf = thinkbayes.make_cdf_from_list(xs)
    print(cdf.percentile(25), cdf.percentile(50), cdf.percentile(75))


def run_estimate(update_func, num_points=31, median_flag=False):
    """Runs the whole analysis.

    update_func: which of the update functions to use
    num_points: number of points in the Suite (in each dimension)
    """
    d = read_heights(nrows=None)
    labels = {1: "male", 2: "female"}

    # PlotCdfs(d, labels)

    suites = {}
    for key, xs in d.items():
        label = labels[key]
        print(label, len(xs))
        summarize(xs)

        xs = thinkbayes.jitter(xs, 1.3)

        mus, sigmas = find_prior_ranges(xs, num_points, median_flag=median_flag)
        suite = Height(mus, sigmas, label)
        suites[label] = suite
        update_func(suite, xs)
        print("MLE", suite.MaximumLikelihood())

        plot_posterior(suite)

        pmf_m = suite.marginal(0)
        pmf_s = suite.marginal(1)
        print("marginal mu", pmf_m.mean(), pmf_m.var())
        print("marginal sigma", pmf_s.mean(), pmf_s.var())

        # PlotMarginals(suite)

    plot_coef_variation(suites)


def main():
    random.seed(17)

    func = update_suite5
    median_flag = func == update_suite5
    run_estimate(func, median_flag=median_flag)


if __name__ == "__main__":
    main()

""" Results:

UpdateSuite1 (100):
marginal mu 162.816901408 0.55779791443
marginal sigma 6.36966103214 0.277026082819

UpdateSuite2 (100):
marginal mu 162.816901408 0.55779791443
marginal sigma 6.36966103214 0.277026082819

UpdateSuite3 (100):
marginal mu 162.816901408 0.55779791443
marginal sigma 6.36966103214 0.277026082819

UpdateSuite4 (100):
marginal mu 162.816901408 0.547456009605
marginal sigma 6.30305516111 0.27544106054

UpdateSuite3 (1000):
marginal mu 163.722137405 0.0660294386397
marginal sigma 6.64453251495 0.0329935312671

UpdateSuite4 (1000):
marginal mu 163.722137405 0.0658920503302
marginal sigma 6.63692197049 0.0329689887609

UpdateSuite3 (all):
marginal mu 163.223475005 0.000203282582659
marginal sigma 7.26918836916 0.000101641131229

UpdateSuite4 (all):
marginal mu 163.223475004 0.000203281499857
marginal sigma 7.26916693422 0.000101640932082

UpdateSuite5 (all):
marginal mu 163.1805214 7.9399898468e-07
marginal sigma 7.29969524118 3.26257030869e-14

"""
