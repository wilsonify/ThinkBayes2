"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging
import math
import sys

import matplotlib.pyplot as pyplot
import numpy as np
import thinkbayes
from thinkbayes import thinkplot

INTERVAL = 245 / 365.0
FORMATS = ["pdf", "eps"]
MINSIZE = 0.2
MAXSIZE = 20
BUCKET_FACTOR = 10
DIAMETER_LABEL = "diameter (cm, log scale)"
TUMOR_LABEL = "tumor age (years)"


def log2(x, denom=math.log(2)):
    """Computes log base 2."""
    return math.log(x) / denom


def simple_model():
    """Runs calculations based on a simple model."""

    # time between discharge and diagnosis, in days
    interval = 3291.0

    # doubling time in linear measure is doubling time in volume * 3
    dt = 811.0 * 3

    # number of doublings since discharge
    doublings = interval / dt

    # how big was the tumor at time of discharge (diameter in cm)
    d1 = 15.5
    d0 = d1 / 2.0 ** doublings

    print(("interval (days)", interval))
    print(("interval (years)", interval / 365))
    print(("dt", dt))
    print(("doublings", doublings))
    print(("d1", d1))
    print(("d0", d0))

    # assume an initial linear measure of 0.1 cm
    d0 = 0.1
    d1 = 15.5

    # how many doublings would it take to get from d0 to d1
    doublings = log2(d1 / d0)

    # what linear doubling time does that imply?
    dt = interval / doublings

    print(("doublings", doublings))
    print(("dt", dt))

    # compute the volumetric doubling time and RDT
    vdt = dt / 3
    rdt = 365 / vdt

    print(("vdt", vdt))
    print(("rdt", rdt))

    cdf = make_cdf()
    p = cdf.prob(rdt)
    print(("Prob{RDT > 2.4}", 1 - p))


def make_cdf():
    """Uses the data from Zhang et al. to construct a CDF."""
    n = 53.0
    freqs = [0, 2, 31, 42, 48, 51, 52, 53]
    ps = [freq / n for freq in freqs]
    xs = np.arange(-1.5, 6.5, 1.0)

    cdf = thinkbayes.Cdf(xs, ps)
    return cdf


def plot_cdf(cdf):
    """Plots the actual and fitted distributions.

    cdf: CDF object
    """
    xs, ps = cdf.xs, cdf.ps
    cps = [1 - p for p in ps]

    # CCDF on logy scale: shows exponential behavior
    thinkplot.clear_figure()
    thinkplot.plot_line(xs, cps, "bo-")
    thinkplot.save_plot(
        root="kidney1",
        formats=FORMATS,
        xlabel="RDT",
        ylabel="CCDF (log scale)",
        yscale="log",
    )

    # CDF, model and data

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)
    mxs, mys = model_cdf()
    thinkplot.plot_line(mxs, mys, label="model", linestyle="dashed")

    thinkplot.plot_line(xs, ps, "gs", label="data")
    thinkplot.save_plot(
        root="kidney2",
        formats=FORMATS,
        xlabel="RDT (volume doublings per year)",
        ylabel="CDF",
        title="Distribution of RDT",
        axis=[-2, 7, 0, 1],
        loc=4,
    )


def qq_plot(cdf, fit):
    """Makes a QQPlot of the values from actual and fitted distributions.

    cdf: actual Cdf of RDT
    fit: model
    """
    xs = [-1.5, 5.5]
    thinkplot.clear_figure()
    thinkplot.plot_line(xs, xs, "b-")

    xs, ps = cdf.xs, cdf.ps
    fs = [fit.value(p) for p in ps]

    thinkplot.plot_line(xs, fs, "gs")
    thinkplot.save_plot(root="kidney3", formats=FORMATS, xlabel="Actual", ylabel="Model")


def fit_cdf(cdf):
    """Fits a line to the log CCDF and returns the slope.

    cdf: Cdf of RDT
    """
    xs, ps = cdf.xs, cdf.ps
    cps = [1 - p for p in ps]

    xs = xs[1:-1]
    lcps = [math.log(p) for p in cps[1:-1]]

    _inter, slope = thinkbayes.least_squares(xs, lcps)
    return -slope


def correlated_generator(cdf, rho):
    """Generates a sequence of values from cdf with correlation.

    Generates a correlated standard Normal series, then transforms to
    values from cdf

    cdf: distribution to choose from
    rho: target coefficient of correlation
    """

    def transform(location):
        """Maps from a Normal variate to a variate with the given CDF."""
        p = thinkbayes.standard_normal_cdf(location)
        y = cdf.value(p)
        return y

    # for the first value, choose from a Normal and transform it
    x = np.random.normal(0, 1)
    yield transform(x)

    # for subsequent values, choose from the conditional distribution
    # based on the previous value
    sigma = math.sqrt(1 - rho ** 2)
    while True:
        x = np.random.normal(x * rho, sigma)
        yield transform(x)


def uncorrelated_generator(cdf, _rho=None):
    """Generates a sequence of values from cdf with no correlation.

    Ignores rho, which is accepted as a parameter to provide the
    same interface as CorrelatedGenerator

    cdf: distribution to choose from
    rho: ignored
    """
    while True:
        x = cdf.random()
        yield x


def rdt_generator(cdf, rho):
    """Returns an iterator with n values from cdf and the given correlation.

    cdf: Cdf object
    rho: coefficient of correlation
    """
    if rho == 0.0:
        return uncorrelated_generator(cdf)
    else:
        return correlated_generator(cdf, rho)


def generate_rdt(pc, lam1, lam2):
    """Generate an RDT from a mixture of exponential distributions.

    With prob pc, generate a negative value with param lam2;
    otherwise generate a positive value with param lam1.
    """
    if np.random.random() < pc:
        return -np.random.exponential(1 / lam2)
    else:
        return np.random.exponential(1 / lam1)


def generate_sample(n, pc, lam1, lam2):
    """Generates a sample of RDTs.

    n: sample size
    pc: probablity of negative growth
    lam1: exponential parameter of positive growth
    lam2: exponential parameter of negative growth

    Returns: list of random variates
    """
    xs = [generate_rdt(pc, lam1, lam2) for _ in range(n)]
    return xs


def generate_cdf(n=1000, pc=0.35, lam1=0.79, lam2=5.0):
    """Generates a sample of RDTs and returns its CDF.

    n: sample size
    pc: probablity of negative growth
    lam1: exponential parameter of positive growth
    lam2: exponential parameter of negative growth

    Returns: Cdf of generated sample
    """
    xs = generate_sample(n, pc, lam1, lam2)
    cdf = thinkbayes.make_cdf_from_list(xs)
    return cdf


def model_cdf(pc=0.35, lam1=0.79, lam2=5.0):
    """

    pc: probablity of negative growth
    lam1: exponential parameter of positive growth
    lam2: exponential parameter of negative growth

    Returns: list of xs, list of ys
    """
    cdf = thinkbayes.eval_exponential_cdf
    x1 = np.arange(-2, 0, 0.1)
    y1 = [pc * (1 - cdf(-x, lam2)) for x in x1]
    x2 = np.arange(0, 7, 0.1)
    y2 = [pc + (1 - pc) * cdf(x, lam1) for x in x2]
    return list(x1) + list(x2), y1 + y2


def bucket_to_cm(y, factor=BUCKET_FACTOR):
    """Computes the linear dimension for a given bucket.

    t: bucket number
    factor: multiplicitive factor from one bucket to the next

    Returns: linear dimension in cm
    """
    return math.exp(y / factor)


def cm_to_bucket(x, factor=BUCKET_FACTOR):
    """Computes the bucket for a given linear dimension.

    x: linear dimension in cm
    factor: multiplicitive factor from one bucket to the next

    Returns: float bucket number
    """
    return round(factor * math.log(x))


def diameter(volume_sphere, factor=3 / math.pi / 4, exp=1 / 3.0):
    """Converts a volume to a diameter.

    d = 2r = 2 * (3/4/pi V)^1/3
    """
    return 2 * (factor * volume_sphere) ** exp


def volume(diameter_sphere, factor=4 * math.pi / 3):
    """Converts a diameter to a volume.

    V = 4/3 pi (d/2)^3
    """
    return factor * (diameter_sphere / 2.0) ** 3


class Cache(object):
    """Records each observation point for each tumor."""

    def __init__(self):
        """Initializes the cache.

        joint: map from (age, bucket) to frequency
        sequences: map from bucket to a list of sequences
        initial_rdt: sequence of (V0, rdt) pairs
        """
        self.joint = thinkbayes.Joint()
        self.sequences = {}
        self.initial_rdt = []

    def get_buckets(self):
        """Returns an iterator for the keys in the cache."""
        return self.sequences.keys()

    def get_sequence(self, bucket):
        """Looks up a bucket in the cache."""
        return self.sequences[bucket]

    def conditional_cdf(self, bucket, name=""):
        """Forms the cdf of ages for a given bucket.

        bucket: int bucket number
        name: string
        """
        pmf = self.joint.conditional(0, 1, bucket, label=name)
        cdf = pmf.make_cdf()
        return cdf

    def prob_older(self, cm, age):
        """Computes the probability of exceeding age, given size.

        cm: size in cm
        age: age in years
        """
        bucket = cm_to_bucket(cm)
        cdf = self.conditional_cdf(bucket)
        p = cdf.prob(age)
        return 1 - p

    def get_dist_age_size(self, size_thresh=MAXSIZE):
        """Gets the joint distribution of age and size.

        Map from (age, log size in cm) to log freq
        
        Returns: new Pmf object
        """
        joint = thinkbayes.Joint()

        for val, freq in self.joint.items():
            age, bucket = val
            cm = bucket_to_cm(bucket)
            if cm > size_thresh:
                continue
            log_cm = math.log10(cm)
            joint.set((age, log_cm), math.log(freq) * 10)

        return joint

    def add_observation(self, age, seq, rdt):
        """Adds this observation point to the cache.

        age: age of the tumor in years
        seq: sequence of volumes
        rdt: RDT during this interval
        """
        final = seq[-1]
        cm = diameter(final)
        bucket = cm_to_bucket(cm)
        self.joint.incr((age, bucket))

        self.sequences.setdefault(bucket, []).append(seq)

        initial = seq[-2]
        self.initial_rdt.append((initial, rdt))

    def print_size(self):
        """Prints the size (cm) for each bucket, and the number of sequences."""
        for bucket in sorted(self.get_buckets()):
            ss = self.get_sequence(bucket)
            _diameter = bucket_to_cm(bucket)
            print((_diameter, len(ss)))

    def correlation(self):
        """Computes the correlation between log volumes and rdts."""
        vs, rdts = zip(*self.initial_rdt)
        lvs = [math.log(v) for v in vs]
        return thinkbayes.corr(lvs, rdts)


class Calculator(object):
    """Encapsulates the state of the computation."""

    def __init__(self):
        """Initializes the cache."""
        self.cache = Cache()

    def make_sequences(self, n, rho, cdf):
        """Returns a list of sequences of volumes.

        n: number of sequences to make
        rho: serial correlation
        cdf: Cdf of rdts

        Returns: list of n sequences of volumes
        """
        sequences = []
        for i in range(n):
            rdt_seq = rdt_generator(cdf, rho)
            seq = self.make_sequence(rdt_seq)
            sequences.append(seq)

            if i % 100 == 0:
                print(i)

        return sequences

    def make_sequence(self, rdt_seq, v0=0.01, interval=INTERVAL, vmax=volume(MAXSIZE)):
        """Simulate the growth of a tumor.

        rdt_seq: sequence of rdts
        v0: initial volume in mL (cm^3)
        interval: timestep in years
        vmax: volume to stop at

        Returns: sequence of volumes
        """
        seq = (v0,)
        age = 0

        for rdt in rdt_seq:
            age += interval
            final, seq = self.extend_sequence(age, seq, rdt, interval)
            if final > vmax:
                break

        return seq

    def extend_sequence(self, age, seq, rdt, interval):
        """Generates a new random value and adds it to the end of seq.

        Side-effect: adds sub-sequences to the cache.

        age: age of tumor at the end of this interval
        seq: sequence of values so far
        rdt: reciprocal doubling time in doublings per year
        interval: timestep in years

        Returns: final volume, extended sequence
        """
        initial = seq[-1]
        doublings = rdt * interval
        final = initial * 2 ** doublings
        new_seq = seq + (final,)
        self.cache.add_observation(age, new_seq, rdt)

        return final, new_seq

    def plot_bucket(self, bucket, color="blue"):
        """Plots the set of sequences for the given bucket.

        bucket: int bucket number
        color: string
        """
        sequences = self.cache.get_sequence(bucket)
        for seq in sequences:
            n = len(seq)
            age = n * INTERVAL
            ts = np.linspace(-age, 0, n)
            plot_sequence(ts, seq, color)

    def plot_buckets(self, buckets=None):
        """Plots the set of sequences that ended in a given bucket."""
        # 2.01, 4.95 cm, 9.97 cm
        if buckets is None:
            buckets = [7.0, 16.0, 23.0]
            logging.debug("%r", f"buckets={buckets}")
            buckets = [23.0]
        logging.debug("%r", f"buckets={buckets}")
        colors = ["blue", "green", "red", "cyan"]

        thinkplot.clear_figure()
        for bucket, color in zip(buckets, colors):
            self.plot_bucket(bucket, color)

        thinkplot.save_plot(
            root="kidney5",
            formats=FORMATS,
            title="History of simulated tumors",
            axis=[-40, 1, MINSIZE, 12],
            xlabel="years",
            ylabel=DIAMETER_LABEL,
            yscale="log",
        )

    def plot_joint_dist(self):
        """Makes a pcolor plot of the age-size joint distribution."""
        thinkplot.clear_figure()

        joint = self.cache.get_dist_age_size()
        thinkplot.contour_plot(joint, contour_bool=False, pcolor_bool=True)

        thinkplot.save_plot(
            root="kidney8",
            formats=FORMATS,
            axis=[0, 41, -0.7, 1.31],
            yticks=make_log_ticks([0.2, 0.5, 1, 2, 5, 10, 20]),
            xlabel="ages",
            ylabel=DIAMETER_LABEL,
        )

    def plot_conditional_cdfs(self):
        """Plots the cdf of ages for each bucket."""
        buckets = [7.0, 16.0, 23.0, 27.0]
        # 2.01, 4.95 cm, 9.97 cm, 14.879 cm
        names = ["2 cm", "5 cm", "10 cm", "15 cm"]
        cdfs = []

        for bucket, name in zip(buckets, names):
            cdf = self.cache.conditional_cdf(bucket, name)
            cdfs.append(cdf)

        thinkplot.clear_figure()
        thinkplot.pre_plot(num=len(cdfs))
        thinkplot.plot_cdfs(cdfs)

        thinkplot.save_plot(
            root="kidney6",
            title="Distribution of age for several diameters",
            formats=FORMATS,
            xlabel=TUMOR_LABEL,
            ylabel="CDF",
            loc=4,
        )

    def plot_credible_intervals(self, xscale="linear"):
        """Plots the confidence interval for each bucket."""
        xs = []
        ts = []
        percentiles = [95, 75, 50, 25, 5]
        min_size = 0.3

        # loop through the buckets, accumulate
        # xs: sequence of sizes in cm
        # ts: sequence of percentile tuples
        for _, bucket in enumerate(sorted(self.cache.get_buckets())):
            cm = bucket_to_cm(bucket)
            if cm < min_size or cm > 20.0:
                continue
            xs.append(cm)
            cdf = self.cache.conditional_cdf(bucket)
            ps = [cdf.percentile(p) for p in percentiles]
            ts.append(ps)

        # dump the results into a table
        fp = open("kidney_table.tex", "w")
        print_table(fp, xs, ts)
        fp.close()

        # make the figure
        linewidths = [1, 2, 3, 2, 1]
        alphas = [0.3, 0.5, 1, 0.5, 0.3]
        labels = ["95th", "75th", "50th", "25th", "5th"]

        # transpose the ts so we have sequences for each percentile rank
        thinkplot.clear_figure()
        yys = zip(*ts)

        for ys, linewidth, alpha, label in zip(yys, linewidths, alphas, labels):
            options = dict(
                color="blue",
                linewidth=linewidth,
                alpha=alpha,
                label=label,
                markersize=2,
            )

            # plot the data points
            thinkplot.plot_line(xs, ys, "bo", **options)

            # plot the fit lines
            fxs = [min_size, 20.0]
            fys = fit_line(xs, ys, fxs)

            thinkplot.plot_line(fxs, fys, **options)

            # put a label at the end of each line
            x, y = fxs[-1], fys[-1]
            pyplot.text(
                x * 1.05,
                y,
                label,
                color="blue",
                horizontalalignment="left",
                verticalalignment="center",
            )

        # make the figure
        thinkplot.save_plot(
            root="kidney7",
            formats=FORMATS,
            title="Credible interval for age vs diameter",
            xlabel=DIAMETER_LABEL,
            ylabel="tumor age (years)",
            xscale=xscale,
            xticks=make_ticks([0.5, 1, 2, 5, 10, 20]),
            axis=[0.25, 35, 0, 45],
            legend=False,
        )


def plot_sequences(sequences):
    """Plots linear measurement vs time.

    sequences: list of sequences of volumes
    """
    thinkplot.clear_figure()

    options = dict(color="gray", linewidth=1, linestyle="dashed")
    thinkplot.plot_line([0, 40], [10, 10], **options)

    for seq in sequences:
        n = len(seq)
        age = n * INTERVAL
        ts = np.linspace(0, age, n)
        plot_sequence(ts, seq)

    thinkplot.save_plot(
        root="kidney4",
        formats=FORMATS,
        axis=[0, 40, MINSIZE, 20],
        title="Simulations of tumor growth",
        xlabel="tumor age (years)",
        yticks=make_ticks([0.2, 0.5, 1, 2, 5, 10, 20]),
        ylabel=DIAMETER_LABEL,
        yscale="log",
    )


def plot_sequence(ts, seq, color="blue"):
    """Plots a time series of linear measurements.

    ts: sequence of times in years
    seq: sequence of columes
    color: color string
    """
    options = dict(color=color, linewidth=1, alpha=0.2)
    xs = [diameter(v) for v in seq]

    thinkplot.plot_line(ts, xs, **options)


def print_ci(fp, cm, ps):
    """Writes a line in the LaTeX table.

    fp: file pointer
    cm: diameter in cm
    ts: tuples of percentiles
    """
    fp.write("%0.1f" % round(cm, 1))
    for p in reversed(ps):
        fp.write(" & %0.1f " % round(p, 1))
    fp.write(r"\\" "\n")


def print_table(fp, xs, ts):
    """Writes the data in a LaTeX table.

    fp: file pointer
    xs: diameters in cm
    ts: sequence of tuples of percentiles
    """
    fp.write(r"\begin{tabular}{|r||r|r|r|r|r|}" "\n")
    hline_str = r"\hline" "\n"
    fp.write(hline_str)
    fp.write(r"Diameter   & \multicolumn{5}{c|}{Percentiles of age} \\" "\n")
    fp.write(r"(cm)   & 5th & 25th & 50th & 75th & 95th \\" "\n")
    fp.write(hline_str)

    for i, (cm, ps) in enumerate(zip(xs, ts)):
        # print cm, ps
        if i % 3 == 0:
            print_ci(fp, cm, ps)

    fp.write(hline_str)
    fp.write(r"\end{tabular}" "\n")


def fit_line(xs, ys, fxs):
    """Fits a line to the xs and ys, and returns fitted values for fxs.

    Applies a log transform to the xs.

    xs: diameter in cm
    ys: age in years
    fxs: diameter in cm
    """
    lxs = [math.log(x) for x in xs]
    inter, slope = thinkbayes.least_squares(lxs, ys)
    # res = correlation.Residuals(lxs, ys, inter, slope)
    # r2 = correlation.CoefDetermination(ys, res)

    lfxs = [math.log(x) for x in fxs]
    fys = [inter + slope * x for x in lfxs]
    return fys


def make_ticks(xs):
    """Makes a pair of sequences for use as pyplot ticks.

    xs: sequence of floats

    Returns (xs, labels), where labels is a sequence of strings.
    """
    labels = [str(x) for x in xs]
    return xs, labels


def make_log_ticks(xs):
    """Makes a pair of sequences for use as pyplot ticks.

    xs: sequence of floats

    Returns (xs, labels), where labels is a sequence of strings.
    """
    lxs = [math.log10(x) for x in xs]
    labels = [str(x) for x in xs]
    return lxs, labels


def test_correlation(cdf):
    """Tests the correlated generator.

    Makes sure that the sequence has the right distribution and correlation.
    """
    n = 10000
    rho = 0.4

    rdt_seq = correlated_generator(cdf, rho)
    xs = [next(rdt_seq) for _ in range(n)]

    rho2 = thinkbayes.serial_corr(xs)
    print((rho, rho2))
    cdf2 = thinkbayes.make_cdf_from_list(xs)

    thinkplot.plot_cdfs([cdf, cdf2])
    thinkplot.show_plot()


def main(script):
    logging.debug("%r", f"script={script}")
    for size in [1, 5, 10]:
        bucket = cm_to_bucket(size)
        print(("Size, bucket", size, bucket))

    simple_model()

    np.random.seed(17)

    cdf = make_cdf()

    lam1 = fit_cdf(cdf)
    fit = generate_cdf(lam1=lam1)

    # TestCorrelation(fit)

    plot_cdf(cdf)
    # QQPlot(cdf, fit)

    calc = Calculator()
    rho = 0.0
    sequences = calc.make_sequences(100, rho, fit)
    plot_sequences(sequences)

    calc.plot_buckets()

    _ = calc.make_sequences(1900, rho, fit)
    print(("V0-RDT correlation", calc.cache.correlation()))

    print(("15.5 Probability age > 8 year", calc.cache.prob_older(15.5, 8)))
    print(("6.0 Probability age > 8 year", calc.cache.prob_older(6.0, 8)))

    calc.plot_conditional_cdfs()

    calc.plot_credible_intervals(xscale="log")

    calc.plot_joint_dist()


if __name__ == "__main__":
    main(*sys.argv)
