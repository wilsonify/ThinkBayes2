import thinkbayes
from thinkbayes import thinkplot


def test_tinder():
    # Ignore the first few cells for now -- they are experiments I am working on related to the prior.

    mu = 1
    pmf = thinkbayes.make_exponential_pmf(mu, high=1.0)
    thinkplot.plot_pdf_line(pmf)

    active = ""
    # Ignore

    mu = 5
    pmf = thinkbayes.make_exponential_pmf(mu, high=1.0)
    thinkplot.plot_pdf_line(pmf)

    active = ""
    # Ignore

    metapmf = thinkbayes.Pmf()
    for lam, prob in pmf.items():
        if lam == 0:
            continue
        pmf = thinkbayes.make_exponential_pmf(lam, high=30)
        metapmf[pmf] = prob

    interarrival = thinkbayes.make_mixture(metapmf)
    thinkplot.plot_pdf_line(interarrival)

    # Ok, let's start here.  Suppose we know $\lambda$.  We can compute the distribution of interarrival times (times between logins).

    lam = 0.1  # average arrival rate in logins per day
    interarrival = pmf = thinkbayes.make_exponential_pmf(lam, high=90)
    thinkplot.plot_pdf_line(interarrival)

    # If we observe someone, we are more likely to land during a longer interval.

    observed = interarrival.copy()
    for val, prob in observed.items():
        observed[val] *= val
    observed.normalize()

    print(interarrival.mean(), observed.mean())
    thinkplot.plot_pdf_line(observed)

    # If we land during an intererval of duration $x$, the time since last login is uniform between 0 and $x$.  So the distribution of time since last login (`timesince`) is a mixture of uniform distributions.

    metapmf = thinkbayes.Pmf()
    for time, prob in observed.items():
        if time == 0:
            continue
        pmf = thinkbayes.make_uniform_pmf(0, time, 101)
        metapmf[pmf] = prob

    timesince = thinkbayes.make_mixture(metapmf)
    print(timesince.mean())
    thinkplot.plot_pdf_line(timesince)

    # The data is in the form of "time since last login", so we need to be able to look up a time, $t$, and get the probability density at $t$.  But we have a PMF with lots of discrete times in it, so we can't just look it up.  One option: Compute the CDF, generate a sample, and estimate the PDF by KDE:

    cdf = thinkbayes.Cdf(timesince)
    thinkplot.plot_cdf_line(cdf)

    # Get a sample:

    sample = cdf.sample(10000)

    # Estimate the PDF:

    pdf = thinkbayes.EstimatedPdf(sample)
    thinkplot.plot_pdf_line(pdf)

    # Second option: use numerical differentiation to compute the derivative of the CDF, which is the PDF:

    import scipy
    import numpy

    xs = numpy.linspace(0, 90, 101)
    ys = [scipy.misc.derivative(cdf.prob, x) for x in xs]

    # Numerical differentiation is more accurate, especially near zero.  The value at zero is wrong: there are ways we could fix it, but it's not necessary because we won't get zero as data.

    thinkplot.plot(xs, ys)
