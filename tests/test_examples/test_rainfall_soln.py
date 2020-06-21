"""
Think Bayes
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import numpy as np
from scipy.special import gamma
from thinkbayes import MakeMixture
from thinkbayes import Pmf, Cdf, Suite, Joint
from thinkbayes import thinkplot


# ## The rain in Boston problem
#
# According to hydrologists, the distribution of total daily
#   rainfall (for days with rain) is well modeled by a two-parameter
#   gamma distribution.  There are several ways to parameterize the
#   gamma distribution; we'll use the one with a shape parameter, $k$, and
#   a scale parameter, $\theta$, described by this PDF:
#
# $ \mathrm{pdf}(x; k, \theta) = \frac{1}{\Gamma(k)\theta^k} x^{k-1} \exp(-x/\theta) $
#
# where $\Gamma$ is the gamma function.
#
# 1. Evaluate this PDF for $x=2$, $k=3$, and $\theta=2$ using NumPy functions and `scipy.special.gamma`.
#
# 2. Evaluate this PDF using `scipy.stats.gamma`, and confirm that you get the same answer.
#
# 3. During the last three days in the Boston area, we have measured the following rainfalls in inches: 0.78, 0.87, 0.64.
#
# Use this data to compute a joint posterior distributions for the
# parameters of the gamma distribution.
#
# You can use the following priors:
#
# * For $k$, Half-normal with $\sigma = 0.5$.
#
# * For $\theta$, Half-normal with $\sigma = 4$.
#
#
# 4. What are the posterior means for $k$ and $\theta$?
#
# 5. Generate a predictive distribution for the amount of rain we will get tomorrow (assuming that it rains at all).  What is the predictive mean?
#
# ### Solution
#
# First, here's a function to evaluate the gamma PDF.


def test_gamma():
    def gamma_pdf(x, k, theta):
        return x ** (k - 1) * np.exp(-x / theta) / gamma(k) / theta ** k

    # And here's a version using `scipy.stats`, translating from the $k$, $\theta$ parameterization to SciPy's inhumane parameterization.

    from scipy import stats

    def gamma_pdf2(x, k, theta):
        return stats.gamma(k, scale=theta).pdf(x)

    # Evaluting the PDF at a test location...

    x = 2
    k = 3
    theta = 2

    gamma_pdf(x, k, theta)

    # And comparing to the results from SciPy

    gamma_pdf2(x, k, theta)

    # Now here's the `Suite` we'll use to estimate parameters from data.

    class Rainfall(Suite, Joint):
        def Likelihood(self, data, hypo):
            """

            data: observed rainfall
            hypo: k, theta
            """
            k, theta = hypo
            x = data
            like = gamma_pdf(x, k, theta)
            return like

    # For the priors, we'll use a HalfNormal for `k`

    from scipy.stats import norm

    ks = np.linspace(0.01, 2, 101)
    ps = norm(0, 0.5).pdf(ks)
    pmf_k = Pmf(dict(zip(ks, ps)))

    # And a HalfNormal for `theta`

    thetas = np.linspace(0.01, 12, 101)
    ps = norm(0, 4).pdf(thetas)
    pmf_theta = Pmf(dict(zip(thetas, ps)))

    # Now we can initialize the suite.

    from thinkbayes import MakeJoint

    suite = Rainfall(MakeJoint(pmf_k, pmf_theta))

    # And update it.

    data = [0.78, 0.87, 0.64]

    # %time suite.UpdateSet(data)

    # To my surprise, the simple implementation of the PDF using NumPy functions is substantially faster than the [SciPy implementation](https://github.com/scipy/scipy/blob/v1.1.0/scipy/stats/_continuous_distns.py#L2429), which evaluates the log-PDF and then exponentiates it.
    #
    # If there's a good reason for that, it's probably because the numerical behavior is better, but the performance hit is big.
    #
    # Anyway, here's the posterior marginal for `k`:

    post_k = suite.Marginal(0)
    print(post_k.Mean())
    thinkplot.plot_pdf_line(post_k)
    thinkplot.decorate(xlabel="k", ylabel="PDF")

    # And here's the posterior marginal for `theta`

    post_theta = suite.Marginal(1)
    print(post_theta.Mean())
    thinkplot.plot_pdf_line(post_theta)
    thinkplot.decorate(xlabel="theta", ylabel="PDF")

    # To make the predictive distribution, we'll need to make PMF approximations to gamma distributions.

    def make_gamma_pmf(xs, k, theta):
        ps = gamma_pdf(xs, k, theta)
        return Pmf(dict(zip(xs, ps)))

    # Here's a test case.

    xs = np.linspace(0, 20)
    pmf = make_gamma_pmf(xs, 3, 2)
    thinkplot.plot_pdf_line(pmf)

    # Now we can make a mixture of gamma distributions with parameters from the posterior joint distribution.

    xs = np.linspace(0.001, 30, 1001)

    metapmf = Pmf()
    for (k, theta), p in suite.Items():
        pmf = make_gamma_pmf(xs, k, theta)
        metapmf[pmf] = p

    # Here's the posterior predictive distribution.  Since it is so steep near 0, we need a pretty fine grid to get an accurate estimate of the posterior predictive mean (which we'll verify by comparison to the solution from MCMC below).

    pred_pmf = MakeMixture(metapmf)
    print(pred_pmf.Mean())
    thinkplot.plot_pdf_line(pred_pmf)

    # ### Now with PyMC
    #
    # Although I generally like to do grid algorithms first and use them to validate the MCMC solution, this example almost works the other way.  I found it easier to write a demonstrably-correct solution in PyMC3, and I used it to help choose the grid location and resolution.

    from warnings import simplefilter

    simplefilter("ignore", FutureWarning)

    import pymc3 as pm

    # Here's the model in three lines.  The only trick part is translating to yet another parameterization.

    model = pm.Model()

    with model:
        k = pm.HalfNormal("k", 0.5)
        theta = pm.HalfNormal("theta", 4)
        rain = pm.Gamma("rain", alpha=k, beta=1 / theta, observed=data)

    # Sampling worked well enough with the default parameters.

    with model:
        trace = pm.sample()

    # Here are the posterior distributions.

    pm.traceplot(trace)

    pm.plot_posterior(trace)

    # Here are the posterior means.

    trace["k"].mean()

    trace["theta"].mean()

    # ### Predictions
    #
    # Here's the posterior predictive distribution.

    with model:
        pred = pm.sample_ppc(trace)

    # And the posterior predictive mean.

    pred["rain"].mean()

    # Comparing the results from MCMC and the grid algorithm

    cdf = Cdf(pred["rain"].flatten())
    thinkplot.plot_cdf_line(cdf, label="MCMC")
    thinkplot.plot_cdf_line(pred_pmf.make_cdf(), label="Grid")
    thinkplot.decorate(xlabel="Predicted rainfall", ylabel="CDF")

    # Looks good.  The predictive means are not quite the same; the most likely culprit is the resolution of the grid algorithm.
