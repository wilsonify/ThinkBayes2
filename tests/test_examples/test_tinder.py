import thinkbayes
from thinkbayes import thinkplot


def test_tinder():
    # Ignore the first few cells for now -- they are experiments I am working on related to the prior.

    mu = 1
    pmf = thinkbayes.MakeExponentialPmf(mu, high=1.0)
    thinkplot.Pdf(pmf)

    active = ""
    # Ignore

    mu = 5
    pmf = thinkbayes.MakeExponentialPmf(mu, high=1.0)
    thinkplot.Pdf(pmf)

    active = ""
    # Ignore

    metapmf = thinkbayes.Pmf()
    for lam, prob in pmf.Items():
        if lam == 0:
            continue
        pmf = thinkbayes.MakeExponentialPmf(lam, high=30)
        metapmf[pmf] = prob

    interarrival = thinkbayes.MakeMixture(metapmf)
    thinkplot.Pdf(interarrival)

    # Ok, let's start here.  Suppose we know $\lambda$.  We can compute the distribution of interarrival times (times between logins).

    lam = 0.1  # average arrival rate in logins per day
    interarrival = pmf = thinkbayes.MakeExponentialPmf(lam, high=90)
    thinkplot.Pdf(interarrival)

    # If we observe someone, we are more likely to land during a longer interval.

    observed = interarrival.Copy()
    for val, prob in observed.Items():
        observed[val] *= val
    observed.Normalize()

    print(interarrival.Mean(), observed.Mean())
    thinkplot.Pdf(observed)

    # If we land during an intererval of duration $x$, the time since last login is uniform between 0 and $x$.  So the distribution of time since last login (`timesince`) is a mixture of uniform distributions.

    metapmf = thinkbayes.Pmf()
    for time, prob in observed.Items():
        if time == 0:
            continue
        pmf = thinkbayes.MakeUniformPmf(0, time, 101)
        metapmf[pmf] = prob

    timesince = thinkbayes.MakeMixture(metapmf)
    print(timesince.Mean())
    thinkplot.Pdf(timesince)

    # The data is in the form of "time since last login", so we need to be able to look up a time, $t$, and get the probability density at $t$.  But we have a PMF with lots of discrete times in it, so we can't just look it up.  One option: Compute the CDF, generate a sample, and estimate the PDF by KDE:

    cdf = thinkbayes.Cdf(timesince)
    thinkplot.Cdf(cdf)

    # Get a sample:

    sample = cdf.Sample(10000)

    # Estimate the PDF:

    pdf = thinkbayes.EstimatedPdf(sample)
    thinkplot.Pdf(pdf)

    # Second option: use numerical differentiation to compute the derivative of the CDF, which is the PDF:

    import scipy
    import numpy

    xs = numpy.linspace(0, 90, 101)
    ys = [scipy.misc.derivative(cdf.Prob, x) for x in xs]

    # Numerical differentiation is more accurate, especially near zero.  The value at zero is wrong: there are ways we could fix it, but it's not necessary because we won't get zero as data.

    thinkplot.plot(xs, ys)
