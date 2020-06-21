"""
This code is inspired by "Think Bayes" by Allen B. Downey

This file contains class definitions for:

Hist: represents a histogram (map from values to integer frequencies).

Pmf: represents a probability mass function (map from values to probs).

_DictWrapper: private parent class for Hist and Pmf.

Cdf: represents a discrete cumulative distribution function

Pdf: represents a continuous probability density function

"""

import bisect
import copy
import logging
import re
from collections import Counter
from io import open
from operator import itemgetter

import numpy as np
import pandas as pd
import scipy
from matplotlib import pyplot as plt
from scipy import ndimage
from scipy import stats
from scipy.special import gamma

ROOT2 = np.sqrt(2)


def random_seed(x):
    """Initialize the random and np.random generators.

    x: int seed
    """
    np.random.seed(x)


def odds(p):
    """Computes odds for a given probability.

    Example: p=0.75 means 75 for and 25 against, or 3:1 odds in favor.

    Note: when p=1, the formula for odds divides by zero, which is
    normally undefined.  But I think it is reasonable to define Odds(1)
    to be infinity, so that's what this function does.

    p: float 0-1

    Returns: float odds
    """
    if p == 1:
        return float("inf")
    return p / (1 - p)


def probability(o):
    """Computes the probability corresponding to given odds.

    Example: o=2 means 2:1 odds in favor, or 2/3 probability

    o: float odds, strictly positive

    Returns: float probability
    """
    return o / (o + 1)


def probability2(yes, no):
    """Computes the probability corresponding to given odds.

    Example: yes=2, no=1 means 2:1 odds in favor, or 2/3 probability.

    yes, no: int or float odds in favor
    """
    return yes / (yes + no)


class Interpolator(object):
    """Represents a mapping between sorted sequences; performs linear interp.

    Attributes:
        xs: sorted list
        ys: sorted list
    """

    def __init__(self, xs, ys):
        self.xs = xs
        self.ys = ys

    def lookup(self, x):
        """Looks up x and returns the corresponding value of y."""
        return self._bisect(x, self.xs, self.ys)

    def reverse(self, y):
        """Looks up y and returns the corresponding value of x."""
        return self._bisect(y, self.ys, self.xs)

    @staticmethod
    def _bisect(x, xs, ys):
        """Helper function."""
        if x <= xs[0]:
            return ys[0]
        if x >= xs[-1]:
            return ys[-1]
        i = bisect.bisect(xs, x)
        frac = 1.0 * (x - xs[i - 1]) / (xs[i] - xs[i - 1])
        y = ys[i - 1] + frac * 1.0 * (ys[i] - ys[i - 1])
        return y


# When we plot Hist, Pmf and Cdf objects, they don't appear in
# the legend unless we override the default label.
DEFAULT_LABEL = "_nolegend_"


class _DictWrapper(object):
    """An object that contains a dictionary."""

    def __init__(self, obj=None, label=None):
        """Initializes the distribution.

        obj: Hist, Pmf, Cdf, Pdf, dict, pandas Series, list of pairs
        label: string label
        """
        self.label = label if label is not None else DEFAULT_LABEL
        self.d = {}

        # flag whether the distribution is under a log transform
        self.log_transformed = False

        if obj is None:
            return

        if isinstance(obj, (_DictWrapper, Cdf, Pdf)):
            self.label = label if label is not None else obj.label

        if isinstance(obj, dict):
            self.d.update(obj.items())
        elif isinstance(obj, (_DictWrapper, Cdf, Pdf)):
            self.d.update(obj.items())
        elif isinstance(obj, pd.Series):
            self.d.update(obj.value_counts().items())
        else:
            # finally, treat it like a list
            self.d.update(Counter(obj))

    def __hash__(self):
        return id(self)

    def __str__(self):
        cls = self.__class__.__name__
        if self.label == DEFAULT_LABEL:
            return f"{cls} {self.d})"
        else:
            return self.label

    def __repr__(self):
        cls = self.__class__.__name__
        if self.label == DEFAULT_LABEL:
            return f"{cls}({repr(self.d)})"
        else:
            return f"{cls}({repr(self.d)}, {repr(self.label)})"

    def __eq__(self, other):
        try:
            return self.d == other.d
        except AttributeError:
            return False

    def __len__(self):
        return len(self.d)

    def __iter__(self):
        return iter(self.d)

    def iterkeys(self):
        """Returns an iterator over keys."""
        return iter(self.d)

    def __contains__(self, value):
        return value in self.d

    def __getitem__(self, value):
        return self.d.get(value, 0)

    def __setitem__(self, value, prob):
        self.d[value] = prob

    def __delitem__(self, value):
        del self.d[value]

    def copy(self, label=None):
        """Returns a copy.

        Make a shallow copy of d.  If you want a deep copy of d,
        use copy.deepcopy on the whole object.

        label: string label for the new Hist

        returns: new _DictWrapper with the same type
        """
        new = copy.copy(self)
        new.d = copy.copy(self.d)
        new.label = label if label is not None else self.label
        return new

    def scale(self, factor):
        """Multiplies the values by a factor.

        factor: what to multiply by

        Returns: new object
        """
        new = self.copy()
        new.d.clear()

        for val, prob in self.items():
            new.set(val * factor, prob)
        return new

    def log(self, m=None):
        """Log transforms the probabilities.

        Removes values with probability 0.

        Normalizes so that the largest logprob is 0.
        """
        if self.log_transformed:
            raise ValueError("Pmf/Hist already under a log transform")
        self.log_transformed = True

        if m is None:
            m = self.max_like()

        for x, p in self.d.items():
            if p:
                self.set(x, np.log(p / m))
            else:
                self.remove(x)

    def exp(self, m=None):
        """Exponentiates the probabilities.

        m: how much to shift the ps before exponentiating

        If m is None, normalizes so that the largest prob is 1.
        """
        if not self.log_transformed:
            raise ValueError("Pmf/Hist not under a log transform")
        self.log_transformed = False

        if m is None:
            m = self.max_like()

        for x, p in self.d.items():
            self.set(x, np.exp(p - m))

    def values(self):
        """Gets an unsorted sequence of values.

        Note: one source of confusion is that the keys of this
        dictionary are the values of the Hist/Pmf, and the
        values of the dictionary are frequencies/probabilities.
        """
        return self.d.keys()

    def items(self):
        """Gets an unsorted sequence of (value, freq/prob) pairs."""
        return self.d.items()

    def sorted_items(self):
        """Gets a sorted sequence of (value, freq/prob) pairs.

        It items are unsortable, the result is unsorted.
        """

        def isnan(x):
            try:
                return np.isnan(x)
            except TypeError:
                return False

        if np.any([isnan(x) for x in self.values()]):
            msg = "Keys contain NaN, may not sort correctly."
            logging.warning(msg)

        try:
            return sorted(self.d.items())
        except TypeError:
            return self.d.items()

    def render(self):
        """Generates a sequence of points suitable for plotting.

        Returns:
            tuple of (sorted value sequence, freq/prob sequence)
        """
        return zip(*self.sorted_items())

    def make_cdf(self, label=None):
        """Makes a Cdf."""
        label = label if label is not None else self.label
        return Cdf(self, label=label)

    def print(self):
        """Prints the values and freqs/probs in ascending order."""
        for val, prob in self.sorted_items():
            print(val, prob)

    def set(self, x, y=0):
        """Sets the freq/prob associated with the value x.

        Args:
            x: number value
            y: number freq or prob
        """
        self.d[x] = y

    def incr(self, x, term=1):
        """Increments the freq/prob associated with the value x.

        Args:
            x: number value
            term: how much to increment by
        """
        self.d[x] = self.d.get(x, 0) + term

    def mult(self, x, factor):
        """Scales the freq/prob associated with the value x.

        Args:
            x: number value
            factor: how much to multiply by
        """
        self.d[x] = self.d.get(x, 0) * factor

    def remove(self, x):
        """Removes a value.

        Throws an exception if the value is not there.

        Args:
            x: value to remove
        """
        del self.d[x]

    def total(self):
        """Returns the total of the frequencies/probabilities in the map."""
        total = sum(self.d.values())
        return total

    def max_like(self):
        """Returns the largest frequency/probability in the map."""
        return max(self.d.values())

    def largest(self, n=10):
        """Returns the largest n values, with frequency/probability.

        n: number of items to return
        """
        return sorted(self.d.items(), reverse=True)[:n]

    def smallest(self, n=10):
        """Returns the smallest n values, with frequency/probability.

        n: number of items to return
        """
        return sorted(self.d.items(), reverse=False)[:n]


class Hist(_DictWrapper):
    """Represents a histogram, which is a map from values to frequencies.

    Values can be any hashable type; frequencies are integer counters.
    """

    def freq(self, x):
        """Gets the frequency associated with the value x.

        Args:
            x: number value

        Returns:
            int frequency
        """
        return self.d.get(x, 0)

    def freqs(self, xs):
        """Gets frequencies for a sequence of values."""
        return [self.freq(x) for x in xs]

    def is_subset(self, other):
        """Checks whether the values in this histogram are a subset of
        the values in the given histogram."""
        for val, freq in self.items():
            if freq > other.freq(val):
                return False
        return True

    def subtract(self, other):
        """Subtracts the values in the given histogram from this histogram."""
        for val, freq in other.items():
            self.incr(val, -freq)


class Pmf(_DictWrapper):
    """Represents a probability mass function.

    Values can be any hashable type; probabilities are floating-point.
    Pmfs are not necessarily normalized.
    """

    def __init__(self, obj=None, label=None):
        super().__init__(obj, label)
        if len(self) > 0:
            self.normalize()

    def prob(self, x, default=0):
        """Gets the probability associated with the value x.

        Args:
            x: number value
            default: value to return if the key is not there

        Returns:
            float probability
        """
        return self.d.get(x, default)

    def probs(self, xs):
        """Gets probabilities for a sequence of values."""
        return [self.prob(x) for x in xs]

    def percentile(self, percentage):
        """Computes a percentile of a given Pmf.

        Note: this is not super efficient.  If you are planning
        to compute more than a few percentiles, compute the Cdf.

        percentage: float 0-100

        returns: value from the Pmf
        """
        p = percentage / 100
        total = 0
        for val, prob in sorted(self.items()):
            total += prob
            if total >= p:
                return val

    def prob_greater(self, x):
        """Probability that a sample from this Pmf exceeds x.

        x: number

        returns: float probability
        """
        if isinstance(x, _DictWrapper):
            return pmf_prob_greater(self, x)
        else:
            t = [prob for (val, prob) in self.d.items() if val > x]
            return sum(t)

    def prob_less(self, x):
        """Probability that a sample from this Pmf is less than x.

        x: number

        returns: float probability
        """
        if isinstance(x, _DictWrapper):
            return pmf_prob_less(self, x)
        else:
            t = [prob for (val, prob) in self.d.items() if val < x]
            return sum(t)

    def prob_equal(self, x):
        """Probability that a sample from this Pmf is exactly x.

        x: number

        returns: float probability
        """
        if isinstance(x, _DictWrapper):
            return pmf_prob_equal(self, x)
        else:
            return self[x]

    # NOTE: I've decided to remove the magic comparators because they
    # have the side-effect of making Pmf sortable, but in fact they
    # don't support sorting.

    def normalize(self, fraction=1):
        """Normalizes this PMF so the sum of all probs is fraction.

        Args:
            fraction: what the total should be after normalization

        Returns: the total probability before normalizing
        """
        if self.log_transformed:
            raise ValueError("Normalize: Pmf is under a log transform")

        total = self.total()
        if total == 0:
            raise ValueError("Normalize: total probability is zero.")

        factor = fraction / total
        for x in self.d:
            self.d[x] *= factor

        return total

    def random(self):
        """Chooses a random element from this PMF.

        Note: this is not very efficient.  If you plan to call
        this more than a few times, consider converting to a CDF.

        Returns:
            float value from the Pmf
        """
        target = np.random.random()
        total = 0
        for x, p in self.d.items():
            total += p
            if total >= target:
                return x

        # we shouldn't get here
        raise ValueError("Random: Pmf might not be normalized.")

    def sample(self, n):
        """Generates a random sample from this distribution.

        n: int length of the sample
        returns: NumPy array
        """
        return self.make_cdf().sample(n)

    def mean(self):
        """Computes the mean of a PMF.

        Returns:
            float mean
        """
        return sum(p * x for x, p in self.items())

    def median(self):
        """Computes the median of a PMF.

        Returns:
            float median
        """
        return self.make_cdf().percentile(50)

    def var(self, mu=None):
        """Computes the variance of a PMF.

        mu: the point around which the variance is computed;
                if omitted, computes the mean

        returns: float variance
        """
        if mu is None:
            mu = self.mean()

        return sum(p * (x - mu) ** 2 for x, p in self.items())

    def expect(self, func):
        """Computes the expectation of func(x).

        Returns:
            expectation
        """
        return np.sum(p * func(x) for x, p in self.items())

    def std(self, mu=None):
        """Computes the standard deviation of a PMF.

        mu: the point around which the variance is computed;
                if omitted, computes the mean

        returns: float standard deviation
        """
        var_around_center = self.var(mu)
        return np.sqrt(var_around_center)

    def mode(self):
        """Returns the value with the highest probability.

        Returns: float probability
        """
        _, val = max((prob, val) for val, prob in self.items())
        return val

    # The mode of a posterior is the maximum aposteori probability (MAP)
    map = mode

    # If the distribution contains likelihoods only, the peak is the
    # maximum likelihood estimator.
    MaximumLikelihood = mode

    def credible_interval(self, percentage=90):
        """Computes the central credible interval.

        If percentage=90, computes the 90% CI.

        Args:
            percentage: float between 0 and 100

        Returns:
            sequence of two floats, low and high
        """
        cdf = self.make_cdf()
        return cdf.credible_interval(percentage)

    def __add__(self, other):
        """Computes the Pmf of the sum of values drawn from self and other.

        other: another Pmf or a scalar

        returns: new Pmf
        """
        try:
            return self.add_pmf(other)
        except AttributeError:
            return self.add_constant(other)

    __radd__ = __add__

    def add_pmf(self, other):
        """Computes the Pmf of the sum of values drawn from self and other.

        other: another Pmf

        returns: new Pmf
        """
        pmf = Pmf()
        for v1, p1 in self.items():
            for v2, p2 in other.items():
                pmf[v1 + v2] += p1 * p2
        return pmf

    def add_constant(self, other):
        """Computes the Pmf of the sum a constant and values from self.

        other: a number

        returns: new Pmf
        """
        if other == 0:
            return self.copy()

        pmf = Pmf()
        for v1, p1 in self.items():
            pmf.set(v1 + other, p1)
        return pmf

    def __sub__(self, other):
        """Computes the Pmf of the diff of values drawn from self and other.

        other: another Pmf

        returns: new Pmf
        """
        try:
            return self.sub_pmf(other)
        except AttributeError:
            return self.add_constant(-other)

    def sub_pmf(self, other):
        """Computes the Pmf of the diff of values drawn from self and other.

        other: another Pmf

        returns: new Pmf
        """
        pmf = Pmf()
        for v1, p1 in self.items():
            for v2, p2 in other.items():
                pmf.incr(v1 - v2, p1 * p2)
        return pmf

    def __mul__(self, other):
        """Computes the Pmf of the product of values drawn from self and other.

        other: another Pmf

        returns: new Pmf
        """
        try:
            return self.mul_pmf(other)
        except AttributeError:
            return self.mul_constant(other)

    def mul_pmf(self, other):
        """Computes the Pmf of the diff of values drawn from self and other.

        other: another Pmf

        returns: new Pmf
        """
        pmf = Pmf()
        for v1, p1 in self.items():
            for v2, p2 in other.items():
                pmf.incr(v1 * v2, p1 * p2)
        return pmf

    def mul_constant(self, other):
        """Computes the Pmf of the product of a constant and values from self.

        other: a number

        returns: new Pmf
        """
        pmf = Pmf()
        for v1, p1 in self.items():
            pmf.set(v1 * other, p1)
        return pmf

    def __div__(self, other):
        """Computes the Pmf of the ratio of values drawn from self and other.

        other: another Pmf

        returns: new Pmf
        """
        try:
            return self.div_pmf(other)
        except AttributeError:
            return self.mul_constant(1 / other)

    __truediv__ = __div__

    def div_pmf(self, other):
        """Computes the Pmf of the ratio of values drawn from self and other.

        other: another Pmf

        returns: new Pmf
        """
        pmf = Pmf()
        for v1, p1 in self.items():
            for v2, p2 in other.items():
                pmf.incr(v1 / v2, p1 * p2)
        return pmf

    def max(self, k):
        """Computes the CDF of the maximum of k selections from this dist.

        k: int

        returns: new Cdf
        """
        cdf = self.make_cdf()
        cdf.ps **= k
        return cdf


class Joint(Pmf):
    """Represents a joint distribution.

    The values are sequences (usually tuples)
    """

    def marginal(self, i, label=None):
        """Gets the marginal distribution of the indicated variable.

        i: index of the variable we want

        Returns: Pmf
        """
        pmf = Pmf(label=label)
        for vs, prob in self.items():
            pmf.incr(vs[i], prob)
        return pmf

    def conditional(self, i, j, val, label=None):
        """Gets the conditional distribution of the indicated variable.

        Distribution of vs[i], conditioned on vs[j] = val.

        i: index of the variable we want
        j: which variable is conditioned on
        val: the value the jth variable has to have

        Returns: Pmf
        """
        pmf = Pmf(label=label)
        for vs, prob in self.items():
            if vs[j] != val:
                continue
            pmf.incr(vs[i], prob)

        pmf.normalize()
        return pmf

    def max_like_interval(self, percentage=90):
        """Returns the maximum-likelihood credible interval.

        If percentage=90, computes a 90% CI containing the values
        with the highest likelihoods.

        percentage: float between 0 and 100

        Returns: list of values from the suite
        """
        interval = []
        total = 0

        t = [(prob, val) for val, prob in self.items()]
        t.sort(reverse=True)

        for prob, val in t:
            interval.append(val)
            total += prob
            if total >= percentage / 100:
                break

        return interval


def make_joint(pmf1, pmf2):
    """Joint distribution of values from pmf1 and pmf2.

    Assumes that the PMFs represent independent random variables.

    Args:
        pmf1: Pmf object
        pmf2: Pmf object

    Returns:
        Joint pmf of value pairs
    """
    joint = Joint()
    for v1, p1 in pmf1.items():
        for v2, p2 in pmf2.items():
            joint.set((v1, v2), p1 * p2)
    return joint


def make_hist_from_list(t, label=None):
    """Makes a histogram from an unsorted sequence of values.

    Args:
        t: sequence of numbers
        label: string label for this histogram

    Returns:
        Hist object
    """
    return Hist(t, label=label)


def make_hist_from_dict(d, label=None):
    """Makes a histogram from a map from values to frequencies.

    Args:
        d: dictionary that maps values to frequencies
        label: string label for this histogram

    Returns:
        Hist object
    """
    return Hist(d, label)


def make_pmf_from_list(t, label=None):
    """Makes a PMF from an unsorted sequence of values.

    Args:
        t: sequence of numbers
        label: string label for this PMF

    Returns:
        Pmf object
    """
    return Pmf(t, label=label)


def make_pmf_from_dict(d, label=None):
    """Makes a PMF from a map from values to probabilities.

    Args:
        d: dictionary that maps values to probabilities
        label: string label for this PMF

    Returns:
        Pmf object
    """
    return Pmf(d, label=label)


def make_pmf_from_items(t, label=None):
    """Makes a PMF from a sequence of value-probability pairs

    Args:
        t: sequence of value-probability pairs
        label: string label for this PMF

    Returns:
        Pmf object
    """
    return Pmf(dict(t), label=label)


def make_pmf_from_hist(hist, label=None):
    """Makes a normalized PMF from a Hist object.

    Args:
        hist: Hist object
        label: string label

    Returns:
        Pmf object
    """
    if label is None:
        label = hist.label

    return Pmf(hist, label=label)


def make_mixture(metapmf, label="mix"):
    """Make a mixture distribution.

    Args:
      metapmf: Pmf that maps from Pmfs to probs.
      label: string label for the new Pmf.

    Returns: Pmf object.
    """
    mix = Pmf(label=label)
    for pmf, p1 in metapmf.items():
        for x, p2 in pmf.items():
            mix[x] += p1 * p2
    return mix


def make_uniform_pmf(low, high, n):
    """Make a uniform Pmf.

    low: lowest value (inclusive)
    high: highest value (inclusize)
    n: number of values
    """
    pmf = Pmf()
    for x in np.linspace(low, high, n):
        pmf.set(x, 1)
    pmf.normalize()
    return pmf


class Cdf:
    """Represents a cumulative distribution function.

    Attributes:
        xs: sequence of values
        ps: sequence of probabilities
        label: string used as a graph label.
    """

    def __init__(self, obj=None, ps=None, label=None):
        """Initializes.

        If ps is provided, obj must be the corresponding list of values.

        obj: Hist, Pmf, Cdf, Pdf, dict, pandas Series, list of pairs
        ps: list of cumulative probabilities
        label: string label
        """
        self.label = label if label is not None else DEFAULT_LABEL

        if isinstance(obj, (_DictWrapper, Cdf, Pdf)) and not label:
            self.label = label if label is not None else obj.label

        if obj is None:
            # caller does not provide obj, make an empty Cdf
            self.xs = np.asarray([])
            self.ps = np.asarray([])
            if ps is not None:
                logging.warning("Cdf: can't pass ps without also passing xs.")
            return
        else:
            # if the caller provides xs and ps, just store them
            if ps is not None:
                if isinstance(ps, str):
                    logging.warning("Cdf: ps can't be a string")

                self.xs = np.asarray(obj)
                self.ps = np.asarray(ps)
                return

        # caller has provided just obj, not ps
        if isinstance(obj, Cdf):
            self.xs = copy.copy(obj.xs)
            self.ps = copy.copy(obj.ps)
            return

        if isinstance(obj, _DictWrapper):
            dw = obj
        else:
            dw = Hist(obj)

        if len(dw) == 0:
            self.xs = np.asarray([])
            self.ps = np.asarray([])
            return

        xs, freqs = zip(*sorted(dw.items()))
        self.xs = np.asarray(xs)
        self.ps = np.cumsum(freqs, dtype=np.float)
        self.ps /= self.ps[-1]

    def __str__(self):
        cls = self.__class__.__name__
        if self.label == DEFAULT_LABEL:
            return f"{cls} {self.xs}, {self.ps})"
        else:
            return self.label

    def __repr__(self):
        cls = self.__class__.__name__
        if self.label == DEFAULT_LABEL:
            return f"{cls}({self.xs}, {self.ps})"
        else:
            return f"{cls} {self.xs}, {self.ps}, {self.label})"

    def __len__(self):
        return len(self.xs)

    def __getitem__(self, x):
        return self.prob(x)

    def __setitem__(self, key, xs):
        assert key == "xs", "only xs may be set in cdf"
        self.xs = xs

    def __delitem__(self, key="xs"):
        self.xs = np.asarray([])

    def __eq__(self, other):
        return np.all(self.xs == other.xs) and np.all(self.ps == other.ps)

    def print(self):
        """Prints the values and freqs/probs in ascending order."""
        for val, prob in zip(self.xs, self.ps):
            print(val, prob)

    def copy(self, label=None):
        """Returns a copy of this Cdf.

        label: string label for the new Cdf
        """
        if label is None:
            label = self.label
        return Cdf(list(self.xs), list(self.ps), label=label)

    def make_pmf(self, label=None):
        """Makes a Pmf."""
        if label is None:
            label = self.label
        return Pmf(self, label=label)

    def items(self):
        """Returns a sorted sequence of (value, probability) pairs.

        Note: in Python3, returns an iterator.
        """
        a = self.ps
        b = np.roll(a, 1)
        b[0] = 0
        return zip(self.xs, a - b)

    def shift(self, term):
        """Adds a term to the xs.

        term: how much to add
        """
        new = self.copy()
        # don't use +=, or else an int array + float yields int array
        new.xs = new.xs + term
        return new

    def scale(self, factor):
        """Multiplies the xs by a factor.

        factor: what to multiply by
        """
        new = self.copy()
        # don't use *=, or else an int array * float yields int array
        new.xs = new.xs * factor
        return new

    def prob(self, x):
        """Returns CDF(x), the probability that corresponds to value x.

        Args:
            x: number

        Returns:
            float probability
        """
        if x < self.xs[0]:
            return 0
        index = bisect.bisect(self.xs, x)
        p = self.ps[index - 1]
        return p

    def probs(self, xs):
        """Gets probabilities for a sequence of values.

        xs: any sequence that can be converted to NumPy array

        returns: NumPy array of cumulative probabilities
        """
        xs = np.asarray(xs)
        index = np.searchsorted(self.xs, xs, side="right")
        ps = self.ps[index - 1]
        ps[xs < self.xs[0]] = 0
        return ps

    ProbArray = probs

    def value(self, p):
        """Returns InverseCDF(p), the value that corresponds to probability p.

        Args:
            p: number in the range [0, 1]

        Returns:
            number value
        """
        if p < 0 or p > 1:
            raise ValueError("Probability p must be in range [0, 1]")

        index = bisect.bisect_left(self.ps, p)
        return self.xs[index]

    def values(self, ps=None):
        """Returns InverseCDF(p), the value that corresponds to probability p.

        If ps is not provided, returns all values.

        Args:
            ps: NumPy array of numbers in the range [0, 1]

        Returns:
            NumPy array of values
        """
        if ps is None:
            return self.xs

        ps = np.asarray(ps)
        if np.any(ps < 0) or np.any(ps > 1):
            raise ValueError("Probability p must be in range [0, 1]")

        index = np.searchsorted(self.ps, ps, side="left")
        return self.xs[index]

    ValueArray = values

    def percentile(self, p):
        """Returns the value that corresponds to percentile p.

        Args:
            p: number in the range [0, 100]

        Returns:
            number value
        """
        return self.value(p / 100)

    def percentiles(self, ps):
        """Returns the value that corresponds to percentiles ps.

        Args:
            ps: numbers in the range [0, 100]

        Returns:
            array of values
        """
        ps = np.asarray(ps)
        return self.values(ps / 100)

    def percentile_rank(self, x):
        """Returns the percentile rank of the value x.

        x: potential value in the CDF

        returns: percentile rank in the range 0 to 100
        """
        return self.prob(x) * 100

    def percentile_ranks(self, xs):
        """Returns the percentile ranks of the values in xs.

        xs: potential value in the CDF

        returns: array of percentile ranks in the range 0 to 100
        """
        return self.probs(xs) * 100

    def random(self):
        """Chooses a random value from this distribution."""
        return self.value(np.random.random())

    def sample(self, n):
        """Generates a random sample from this distribution.

        n: int length of the sample
        returns: NumPy array
        """
        ps = np.random.random(n)  # pylint: disable=no-member
        return self.ValueArray(ps)

    def mean(self):
        """Computes the mean of a CDF.

        Returns:
            float mean
        """
        old_p = 0
        total = 0
        for x, new_p in zip(self.xs, self.ps):
            p = new_p - old_p
            total += p * x
            old_p = new_p
        return total

    def credible_interval(self, percentage=90):
        """Computes the central credible interval.

        If percentage=90, computes the 90% CI.

        Args:
            percentage: float between 0 and 100

        Returns:
            sequence of two floats, low and high
        """
        prob = (1 - percentage / 100) / 2
        interval = self.value(prob), self.value(1 - prob)
        return interval

    ConfidenceInterval = credible_interval

    def _round(self, a, b, multiplier=1000):
        """
        An entry is added to the cdf only if the percentile differs
        from the previous value in a significant digit, where the number
        of significant digits is determined by multiplier.  The
        default is 1000, which keeps log10(1000) = 3 significant digits.
        """
        digits = np.log10(multiplier)
        return np.isclose(self.prob(a), self.prob(b), atol=10 ** digits)

    def render(self):
        """Generates a sequence of points suitable for plotting.

        An empirical CDF is a step function; linear interpolation
        can be misleading.

        Returns:
            tuple of (xs, ps)
        """

        def interleave(a_interleave, b_interleave):
            c = np.empty(a_interleave.shape[0] + b_interleave.shape[0])
            c[::2] = a_interleave
            c[1::2] = b_interleave
            return c

        a = np.array(self.xs)
        xs = interleave(a, a)
        shift_ps = np.roll(self.ps, 1)
        shift_ps[0] = 0
        ps = interleave(shift_ps, self.ps)
        return xs, ps

    def max(self, k):
        """Computes the CDF of the maximum of k selections from this dist.

        k: int

        returns: new Cdf
        """
        cdf = self.copy()
        cdf.ps **= k
        return cdf


def make_cdf_from_items(items, label=None):
    """Makes a cdf from an unsorted sequence of (value, frequency) pairs.

    Args:
        items: unsorted sequence of (value, frequency) pairs
        label: string label for this CDF

    Returns:
        cdf: list of (value, fraction) pairs
    """
    return Cdf(dict(items), label=label)


def make_cdf_from_dict(d, label=None):
    """Makes a CDF from a dictionary that maps values to frequencies.

    Args:
       d: dictionary that maps values to frequencies.
       label: string label for the data.

    Returns:
        Cdf object
    """
    return Cdf(d, label=label)


def make_cdf_from_list(seq, label=None):
    """Creates a CDF from an unsorted sequence.

    Args:
        seq: unsorted sequence of sortable values
        label: string label for the cdf

    Returns:
       Cdf object
    """
    return Cdf(seq, label=label)


def make_cdf_from_hist(hist, label=None):
    """Makes a CDF from a Hist object.

    Args:
       hist: Pmf.Hist object
       label: string label for the data.

    Returns:
        Cdf object
    """
    if label is None:
        label = hist.label

    return Cdf(hist, label=label)


def make_cdf_from_pmf(pmf, label=None):
    """Makes a CDF from a Pmf object.

    Args:
       pmf: Pmf.Pmf object
       label: string label for the data.

    Returns:
        Cdf object
    """
    if label is None:
        label = pmf.label

    return Cdf(pmf, label=label)


class UnimplementedMethodException(Exception):
    """Exception if someone calls a method that should be overridden."""


class Suite(Pmf):
    """Represents a suite of hypotheses and their probabilities."""

    def update(self, data):
        """Updates each hypothesis based on the data.

        data: any representation of the data

        returns: the normalizing constant
        """
        logging.debug("%r", f"data={data}")
        for hypo in self.values():
            logging.debug("%r", f"hypo={hypo}")
            like = self.likelihood(data, hypo)
            self.mult(hypo, like)
        return self.normalize()

    def log_update(self, data):
        """Updates a suite of hypotheses based on new data.

        Modifies the suite directly; if you want to keep the original, make
        a copy.

        Note: unlike Update, LogUpdate does not normalize.

        Args:
            data: any representation of the data
        """
        for hypo in self.values():
            like = self.log_likelihood(data, hypo)
            self.incr(hypo, like)

    def update_set(self, dataset):
        """Updates each hypothesis based on the dataset.

        This is more efficient than calling Update repeatedly because
        it waits until the end to Normalize.

        Modifies the suite directly; if you want to keep the original, make
        a copy.

        dataset: a sequence of data

        returns: the normalizing constant
        """
        for data in dataset:
            for hypo in self.values():
                like = self.likelihood(data, hypo)
                self.mult(hypo, like)
        return self.normalize()

    def log_update_set(self, dataset):
        """Updates each hypothesis based on the dataset.

        Modifies the suite directly; if you want to keep the original, make
        a copy.

        dataset: a sequence of data

        returns: None
        """
        for data in dataset:
            self.log_update(data)

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: some representation of the hypothesis
        data: some representation of the data
        """
        raise UnimplementedMethodException()

    def log_likelihood(self, data, hypo):
        """Computes the log likelihood of the data under the hypothesis.

        hypo: some representation of the hypothesis
        data: some representation of the data
        """
        raise UnimplementedMethodException()

    def print(self):
        """Prints the hypotheses and their probabilities."""
        for hypo, prob in sorted(self.items()):
            print(hypo, prob)

    def make_odds(self):
        """Transforms from probabilities to odds.

        Values with prob=0 are removed.
        """
        for hypo, prob in self.items():
            if prob:
                self.set(hypo, odds(prob))
            else:
                self.remove(hypo)

    def make_probs(self):
        """Transforms from odds to probabilities."""
        for hypo, odds_of_hypo in self.items():
            self.set(hypo, probability(odds_of_hypo))


def make_suite_from_list(t, label=None):
    """Makes a suite from an unsorted sequence of values.

    Args:
        t: sequence of numbers
        label: string label for this suite

    Returns:
        Suite object
    """
    hist = make_hist_from_list(t, label=label)
    d = hist.d
    return make_suite_from_dict(d)


def make_suite_from_hist(hist, label=None):
    """Makes a normalized suite from a Hist object.

    Args:
        hist: Hist object
        label: string label

    Returns:
        Suite object
    """
    if label is None:
        label = hist.label

    # make a copy of the dictionary
    d = dict(hist.GetDict())
    return make_suite_from_dict(d, label)


def make_suite_from_dict(d, label=None):
    """Makes a suite from a map from values to probabilities.

    Args:
        d: dictionary that maps values to probabilities
        label: string label for this suite

    Returns:
        Suite object
    """
    suite = Suite(label=label)
    suite.d = d
    suite.normalize()
    return suite


class Pdf(object):
    """Represents a probability density function (PDF)."""

    def density(self, xs):
        """Evaluates this Pdf at x.

        Returns: float or NumPy array of probability density
        """
        raise UnimplementedMethodException()

    def get_linspace(self):
        """Get a linspace for plotting.

        Not all subclasses of Pdf implement this.

        Returns: numpy array
        """
        raise UnimplementedMethodException()

    def make_pmf(self, **options):
        """Makes a discrete version of this Pdf.

        options can include
        label: string
        low: low end of range
        high: high end of range
        n: number of places to evaluate

        Returns: new Pmf
        """
        label = options.pop("label", "")
        xs, ds = self.render(**options)
        return Pmf(dict(zip(xs, ds)), label=label)

    def render(self, **options):
        """Generates a sequence of points suitable for plotting.

        If options includes low and high, it must also include n;
        in that case the density is evaluated an n locations between
        low and high, including both.

        If options includes xs, the density is evaluate at those location.

        Otherwise, self.GetLinspace is invoked to provide the locations.

        Returns:
            tuple of (xs, densities)
        """
        low, high = options.pop("low", None), options.pop("high", None)
        if low is not None and high is not None:
            n = options.pop("n", 101)
            xs = np.linspace(low, high, n)
        else:
            xs = options.pop("xs", None)
            if xs is None:
                xs = self.get_linspace()

        ds = self.density(xs)
        return xs, ds

    def items(self):
        """Generates a sequence of (value, probability) pairs.
        """
        return zip(*self.render())


class NormalPdf(Pdf):
    """Represents the PDF of a Normal distribution."""

    def __init__(self, mu=0, sigma=1, label=None):
        """Constructs a Normal Pdf with given mu and sigma.

        mu: mean
        sigma: standard deviation
        label: string
        """
        self.mu = mu
        self.sigma = sigma
        self.label = label if label is not None else "_nolegend_"

    def __str__(self):
        return f"NormalPdf({self.mu}, {self.sigma})"

    def get_linspace(self):
        """Get a linspace for plotting.

        Returns: numpy array
        """
        low, high = self.mu - 3 * self.sigma, self.mu + 3 * self.sigma
        return np.linspace(low, high, 101)

    def density(self, xs):
        """Evaluates this Pdf at xs.

        xs: scalar or sequence of floats

        returns: float or NumPy array of probability density
        """
        return stats.norm.pdf(xs, self.mu, self.sigma)


class ExponentialPdf(Pdf):
    """Represents the PDF of an exponential distribution."""

    def __init__(self, lam=1, label=None):
        """Constructs an exponential Pdf with given parameter.

        lam: rate parameter
        label: string
        """
        self.lam = lam
        self.label = label if label is not None else "_nolegend_"

    def __str__(self):
        return f"ExponentialPdf({self.lam})"

    def get_linspace(self):
        """Get a linspace for plotting.

        Returns: numpy array
        """
        low, high = 0, 5.0 / self.lam
        return np.linspace(low, high, 101)

    def density(self, xs):
        """Evaluates this Pdf at xs.

        xs: scalar or sequence of floats

        returns: float or NumPy array of probability density
        """
        return stats.expon.pdf(xs, scale=1.0 / self.lam)


class EstimatedPdf(Pdf):
    """Represents a PDF estimated by KDE."""

    def __init__(self, sample, label=None):
        """Estimates the density function based on a sample.

        sample: sequence of data
        label: string
        """
        self.label = label if label is not None else "_nolegend_"
        self.kde = stats.gaussian_kde(sample)
        low = min(sample)
        high = max(sample)
        self.linspace = np.linspace(low, high, 101)

    def __str__(self):
        return f"EstimatedPdf(label={self.label})"

    def get_linspace(self):
        """Get a linspace for plotting.

        Returns: numpy array
        """
        return self.linspace

    def density(self, xs):
        """Evaluates this Pdf at xs.

        returns: float or NumPy array of probability density
        """
        return self.kde.evaluate(xs)

    def sample(self, n):
        """Generates a random sample from the estimated Pdf.

        n: size of sample
        """
        # NOTE: we have to flatten because resample returns a 2-D
        # array for some reason.
        return self.kde.resample(n).flatten()


def credible_interval(pmf, percentage=90):
    """Computes a credible interval for a given distribution.

    If percentage=90, computes the 90% CI.

    Args:
        pmf: Pmf object representing a posterior distribution
        percentage: float between 0 and 100

    Returns:
        sequence of two floats, low and high
    """
    cdf = pmf.make_cdf()
    prob = (1 - percentage / 100) / 2
    interval = cdf.value(prob), cdf.value(1 - prob)
    return interval


def pmf_prob_less(pmf1, pmf2):
    """Probability that a value from pmf1 is less than a value from pmf2.

    Args:
        pmf1: Pmf object
        pmf2: Pmf object

    Returns:
        float probability
    """
    total = 0
    for v1, p1 in pmf1.items():
        for v2, p2 in pmf2.items():
            if v1 < v2:
                total += p1 * p2
    return total


def pmf_prob_greater(pmf1, pmf2):
    """Probability that a value from pmf1 is less than a value from pmf2.

    Args:
        pmf1: Pmf object
        pmf2: Pmf object

    Returns:
        float probability
    """
    total = 0
    for v1, p1 in pmf1.items():
        for v2, p2 in pmf2.items():
            if v1 > v2:
                total += p1 * p2
    return total


def pmf_prob_equal(pmf1, pmf2):
    """Probability that a value from pmf1 equals a value from pmf2.

    Args:
        pmf1: Pmf object
        pmf2: Pmf object

    Returns:
        float probability
    """
    total = 0
    for v1, p1 in pmf1.items():
        for v2, p2 in pmf2.items():
            if v1 == v2:
                total += p1 * p2
    return total


def random_sum(dists):
    """Chooses a random value from each dist and returns the sum.

    dists: sequence of Pmf or Cdf objects

    returns: numerical sum
    """
    total = sum(dist.random() for dist in dists)
    return total


def sample_sum(dists, n):
    """Draws a sample of sums from a list of distributions.

    dists: sequence of Pmf or Cdf objects
    n: sample size

    returns: new Pmf of sums
    """
    pmf = Pmf(random_sum(dists) for _ in range(n))
    return pmf


def eval_normal_pdf(x, mu, sigma):
    """Computes the unnormalized PDF of the normal distribution.

    x: value
    mu: mean
    sigma: standard deviation

    returns: float probability density
    """
    return stats.norm.pdf(x, mu, sigma)


def make_normal_pmf(mu, sigma, num_sigmas, n=201):
    """Makes a PMF discrete approx to a Normal distribution.

    mu: float mean
    sigma: float standard deviation
    num_sigmas: how many sigmas to extend in each direction
    n: number of values in the Pmf

    returns: normalized Pmf
    """
    pmf = Pmf()
    low = mu - num_sigmas * sigma
    high = mu + num_sigmas * sigma

    for x in np.linspace(low, high, n):
        p = eval_normal_pdf(x, mu, sigma)
        pmf.set(x, p)
    pmf.normalize()
    return pmf


def eval_binomial_pmf(k, n, p):
    """Evaluates the binomial PMF.

    k: number of successes
    n: number of trials
    p: probability of success on each trial

    returns: probabily of k successes in n trials with probability p.
    """
    return stats.binom.pmf(k, n, p)


def make_binomial_pmf(n, p):
    """Evaluates the binomial PMF.

    n: number of trials
    p: probability of success on each trial

    returns: Pmf of number of successes
    """
    pmf = Pmf()
    for k in range(n + 1):
        pmf[k] = stats.binom.pmf(k, n, p)
    return pmf


def eval_gamma_pdf(x, a):
    """Computes the Gamma PDF.

    x: where to evaluate the PDF
    a: parameter of the gamma distribution

    returns: float probability
    """
    return x ** (a - 1) * np.exp(-x) / gamma(a)


def make_gamma_pmf(xs, a):
    """Makes a PMF discrete approx to a Gamma distribution.

    lam: parameter lambda in events per unit time
    xs: upper bound of the Pmf

    returns: normalized Pmf
    """
    xs = np.asarray(xs)
    ps = eval_gamma_pdf(xs, a)
    pmf = Pmf(dict(zip(xs, ps)))
    pmf.normalize()
    return pmf


def eval_geometric_pmf(k, p, loc=0):
    """Evaluates the geometric PMF.

    With loc=0: Probability of `k` trials to get one success.
    With loc=-1: Probability of `k` trials before first success.

    k: number of trials
    p: probability of success on each trial
    """
    return stats.geom.pmf(k, p, loc=loc)


def make_geometric_pmf(p, loc=0, high=10):
    """Evaluates the binomial PMF.

    With loc=0: PMF of trials to get one success.
    With loc=-1: PMF of trials before first success.

    p: probability of success
    high: upper bound where PMF is truncated
    """
    pmf = Pmf()
    for k in range(high):
        pmf[k] = stats.geom.pmf(k, p, loc=loc)
    pmf.normalize()
    return pmf


def eval_hypergeom_pmf(k_success, n_poplation, k_population, n_trials):
    """Evaluates the hypergeometric PMF.

    Returns the probabily of k successes in n trials from a population
    N with K successes in it.
    """
    return stats.hypergeom.pmf(k_success, n_poplation, k_population, n_trials)


def eval_poisson_pmf(k, lam):
    """Computes the Poisson PMF.

    k: number of events
    lam: parameter lambda in events per unit time

    returns: float probability
    """
    return stats.poisson.pmf(k, lam)


def make_poisson_pmf(lam, high, step=1):
    """Makes a PMF discrete approx to a Poisson distribution.

    lam: parameter lambda in events per unit time
    high: upper bound of the Pmf

    returns: normalized Pmf
    """
    pmf = Pmf()
    for k in range(0, high + 1, step):
        p = stats.poisson.pmf(k, lam)
        pmf.set(k, p)
    pmf.normalize()
    return pmf


def eval_exponential_pdf(x, lam):
    """Computes the exponential PDF.

    x: value
    lam: parameter lambda in events per unit time

    returns: float probability density
    """
    return lam * np.exp(-lam * x)


def eval_exponential_cdf(x, lam):
    """Evaluates CDF of the exponential distribution with parameter lam."""
    return 1 - np.exp(-lam * x)


def make_exponential_pmf(lam, high, n=200):
    """Makes a PMF discrete approx to an exponential distribution.

    lam: parameter lambda in events per unit time
    high: upper bound
    n: number of values in the Pmf

    returns: normalized Pmf
    """
    pmf = Pmf()
    for x in np.linspace(0, high, n):
        p = eval_exponential_pdf(x, lam)
        pmf.set(x, p)
    pmf.normalize()
    return pmf


def eval_weibull_pdf(x, lam, k):
    """Computes the Weibull PDF.

    x: value
    lam: parameter lambda in events per unit time
    k: parameter

    returns: float probability density
    """
    arg = x / lam
    return k / lam * arg ** (k - 1) * np.exp(-(arg ** k))


def eval_weibull_cdf(x, lam, k):
    """Evaluates CDF of the Weibull distribution."""
    arg = x / lam
    return 1 - np.exp(-(arg ** k))


def make_weibull_pmf(lam, k, high, n=200):
    """Makes a PMF discrete approx to a Weibull distribution.

    lam: parameter lambda in events per unit time
    k: parameter
    high: upper bound
    n: number of values in the Pmf

    returns: normalized Pmf
    """
    xs = np.linspace(0, high, n)
    ps = eval_weibull_pdf(xs, lam, k)
    ps[np.isinf(ps)] = 0
    return Pmf(dict(zip(xs, ps)))


def eval_pareto_pdf(x, xm, alpha):
    """Computes the Pareto.

    xm: minimum value (scale parameter)
    alpha: shape parameter

    returns: float probability density
    """
    return stats.pareto.pdf(x, alpha, scale=xm)


def make_pareto_pmf(xm, alpha, high, num=101):
    """Makes a PMF discrete approx to a Pareto distribution.

    xm: minimum value (scale parameter)
    alpha: shape parameter
    high: upper bound value
    num: number of values

    returns: normalized Pmf
    """
    xs = np.linspace(xm, high, num)
    ps = stats.pareto.pdf(xs, alpha, scale=xm)
    pmf = Pmf(dict(zip(xs, ps)))
    return pmf


def standard_normal_cdf(x):
    """Evaluates the CDF of the standard Normal distribution.

    See https://en.wikipedia.org/wiki/Normal_distribution
    #Cumulative_distribution_function

    Args:
        x: float

    Returns:
        float
    """
    return (np.erf(x / ROOT2) + 1) / 2


def eval_normal_cdf(x, mu=0, sigma=1):
    """Evaluates the CDF of the normal distribution.

    Args:
        x: float

        mu: mean parameter

        sigma: standard deviation parameter

    Returns:
        float
    """
    return stats.norm.cdf(x, loc=mu, scale=sigma)


def eval_normal_cdf_inverse(p, mu=0, sigma=1):
    """Evaluates the inverse CDF of the normal distribution.

    See https://en.wikipedia.org/wiki/Normal_distribution#Quantile_function

    Args:
        p: float

        mu: mean parameter

        sigma: standard deviation parameter

    Returns:
        float
    """
    return stats.norm.ppf(p, loc=mu, scale=sigma)


def eval_lognormal_pdf(x, mu=0, sigma=1):
    """Evaluates the PDF of the lognormal distribution.

    x: float or sequence
    mu: mean parameter
    sigma: standard deviation parameter

    Returns: float or sequence
    """
    return stats.lognorm.pdf(x, loc=mu, scale=sigma)


def eval_lognormal_cdf(x, mu=0, sigma=1):
    """Evaluates the CDF of the lognormal distribution.

    x: float or sequence
    mu: mean parameter
    sigma: standard deviation parameter

    Returns: float or sequence
    """
    return stats.lognorm.cdf(x, loc=mu, scale=sigma)


def render_expo_cdf(lam, low, high, n=101):
    """Generates sequences of xs and ps for an exponential CDF.

    lam: parameter
    low: float
    high: float
    n: number of points to render

    returns: numpy arrays (xs, ps)
    """
    xs = np.linspace(low, high, n)
    ps = 1 - np.exp(-lam * xs)
    # ps = stats.expon.cdf(xs, scale=1.0/lam)
    return xs, ps


def render_normal_cdf(mu, sigma, low, high, n=101):
    """Generates sequences of xs and ps for a Normal CDF.

    mu: parameter
    sigma: parameter
    low: float
    high: float
    n: number of points to render

    returns: numpy arrays (xs, ps)
    """
    xs = np.linspace(low, high, n)
    ps = stats.norm.cdf(xs, mu, sigma)
    return xs, ps


def render_pareto_cdf(xmin, alpha, low, high, n=50):
    """Generates sequences of xs and ps for a Pareto CDF.

    xmin: parameter
    alpha: parameter
    low: float
    high: float
    n: number of points to render

    returns: numpy arrays (xs, ps)
    """
    if low < xmin:
        low = xmin
    xs = np.linspace(low, high, n)
    ps = 1 - (xs / xmin) ** -alpha
    # ps = stats.pareto.cdf(xs, scale=xmin, b=alpha)
    return xs, ps


class Beta:
    """Represents a Beta distribution.

    See https://en.wikipedia.org/wiki/Beta_distribution
    """

    def __init__(self, alpha=1, beta=1, label=None):
        """Initializes a Beta distribution."""
        self.alpha = alpha
        self.beta = beta
        self.label = label if label is not None else "_nolegend_"

    def update(self, data):
        """Updates a Beta distribution.

        data: pair of int (heads, tails)
        """
        heads, tails = data
        self.alpha += heads
        self.beta += tails

    def mean(self):
        """Computes the mean of this distribution."""
        return self.alpha / (self.alpha + self.beta)

    def map(self):
        """Computes the value with Maximum A posteori Probability."""
        a = self.alpha - 1
        b = self.beta - 1
        return a / (a + b)

    def random(self):
        """Generates a random variate from this distribution."""
        return np.random.beta(self.alpha, self.beta)

    def sample(self, n):
        """Generates a random sample from this distribution.

        n: int sample size
        """
        size = n
        return np.random.beta(self.alpha, self.beta, size)

    def eval_pdf(self, x):
        """Evaluates the PDF at x."""
        return x ** (self.alpha - 1) * (1 - x) ** (self.beta - 1)

    def make_pmf(self, steps=101, label=None):
        """Returns a Pmf of this distribution.

        Note: Normally, we just evaluate the PDF at a sequence
        of points and treat the probability density as a probability
        mass.

        But if alpha or beta is less than one, we have to be
        more careful because the PDF goes to infinity at x=0
        and x=1.  In that case we evaluate the CDF and compute
        differences.

        The result is a little funny, because the values at 0 and 1
        are not symmetric.  Nevertheless, it is a reasonable discrete
        model of the continuous distribution, and behaves well as
        the number of values increases.
        """
        if label is None and self.label is not None:
            label = self.label

        if self.alpha < 1 or self.beta < 1:
            cdf = self.make_cdf()
            pmf = cdf.make_pmf()
            return pmf

        xs = [i / (steps - 1.0) for i in range(steps)]
        probs = [self.eval_pdf(x) for x in xs]
        pmf = Pmf(dict(zip(xs, probs)), label=label)
        return pmf

    def make_cdf(self, steps=101):
        """Returns the CDF of this distribution."""
        xs = [i / (steps - 1.0) for i in range(steps)]
        ps = scipy.special.betainc(
            self.alpha, self.beta, xs
        )  # pylint: disable=no-member
        cdf = Cdf(xs, ps)
        return cdf

    def percentile(self, ps):
        """Returns the given percentiles from this distribution.

        ps: scalar, array, or list of [0-100]
        """
        ps = np.asarray(ps) / 100
        xs = scipy.special.betaincinv(
            self.alpha, self.beta, ps
        )  # pylint: disable=no-member
        return xs


class Dirichlet(object):
    """Represents a Dirichlet distribution.

    See https://en.wikipedia.org/wiki/Dirichlet_distribution
    """

    def __init__(self, n, conc=1, label=None):
        """Initializes a Dirichlet distribution.

        n: number of dimensions
        conc: concentration parameter (smaller yields more concentration)
        label: string label
        """
        if n < 2:
            raise ValueError("A Dirichlet distribution with " "n<2 makes no sense")

        self.n = n
        self.params = np.ones(n, dtype=np.float) * conc
        self.label = label if label is not None else "_nolegend_"

    def update(self, data):
        """Updates a Dirichlet distribution.

        data: sequence of observations, in order corresponding to params
        """
        m = len(data)
        self.params[:m] += data

    def random(self):
        """Generates a random variate from this distribution.

        Returns: normalized vector of fractions
        """
        p = np.random.gamma(self.params)
        return p / p.sum()

    def likelihood(self, data):
        """Computes the likelihood of the data.

        Selects a random vector of probabilities from this distribution.

        Returns: float probability
        """
        m = len(data)
        if self.n < m:
            return 0

        x = data
        p = self.random()
        q = p[:m] ** x
        return q.prod()

    def log_likelihood(self, data):
        """Computes the log likelihood of the data.

        Selects a random vector of probabilities from this distribution.

        Returns: float log probability
        """
        m = len(data)
        if self.n < m:
            return float("-inf")

        x = self.random()
        y = np.log(x[:m]) * data
        return y.sum()

    def marginal_beta(self, i):
        """Computes the marginal distribution of the ith element.

        See https://en.wikipedia.org/wiki/Dirichlet_distribution
        #Marginal_distributions

        i: int

        Returns: Beta object
        """
        alpha0 = self.params.sum()
        alpha = self.params[i]
        return Beta(alpha, alpha0 - alpha)

    def predictive_pmf(self, xs, label=None):
        """Makes a predictive distribution.

        xs: values to go into the Pmf

        Returns: Pmf that maps from x to the mean prevalence of x
        """
        alpha0 = self.params.sum()
        ps = self.params / alpha0
        return Pmf(zip(xs, ps), label=label)

    def mean(self):
        """Array of means."""
        return self.params / np.sum(self.params)


def binomial_coef(n, k):
    """Compute the binomial coefficient "n choose k".

    n: number of trials
    k: number of successes

    Returns: float
    """
    return scipy.special.comb(n, k)


def log_binomial_coef(n, k):
    """Computes the log of the binomial coefficient.

    https://math.stackexchange.com/questions/64716/
    approximating-the-logarithm-of-the-binomial-coefficient

    n: number of trials
    k: number of successes

    Returns: float
    """
    return n * np.log(n) - k * np.log(k) - (n - k) * np.log(n - k)


def normal_probability(ys, jitter_scale=0):
    """Generates data for a normal probability plot.

    ys: sequence of values
    jitter: float magnitude of jitter added to the ys

    returns: numpy arrays xs, ys
    """
    n = len(ys)
    xs = np.random.normal(0, 1, n)
    xs.sort()

    if jitter_scale:
        ys = jitter_scale(ys, jitter_scale)
    else:
        ys = np.array(ys)
    ys.sort()

    return xs, ys


def jitter(values, jitter_scale=0.5):
    """Jitters the values by adding a uniform variate in (-jitter, jitter).

    values: sequence
    jitter: scalar magnitude of jitter

    returns: new numpy array
    """
    n = len(values)
    return np.random.normal(0, jitter_scale, n) + values


def normal_probability_plot(sample, fit_color="0.8", **options):
    """Makes a normal probability plot with a fitted line.

    sample: sequence of numbers
    fit_color: color string for the fitted line
    options: passed along to Plot
    """
    xs, ys = normal_probability(sample)
    mean_x, var_x = mean_var(sample)
    std_x = np.sqrt(var_x)

    fit = fit_line(xs, mean_x, std_x)
    plt.plot(*fit, color=fit_color, label="model")

    xs, ys = normal_probability(sample)
    plt.plot(xs, ys, **options)


def mean(xs):
    """Computes mean.

    xs: sequence of values

    returns: float mean
    """
    return np.mean(xs)


def var(xs, mu=None, ddof=0):
    """Computes variance.

    xs: sequence of values
    mu: option known mean
    ddof: delta degrees of freedom

    returns: float
    """
    xs = np.asarray(xs)

    if mu is None:
        mu = xs.mean()

    ds = xs - mu
    return np.dot(ds, ds) / (len(xs) - ddof)


def std(xs, mu=None, ddof=0):
    """Computes standard deviation.

    xs: sequence of values
    mu: option known mean
    ddof: delta degrees of freedom

    returns: float
    """
    var_x = var(xs, mu, ddof)
    return np.sqrt(var_x)


def mean_var(xs, ddof=0):
    """Computes mean and variance.

    Based on https://stackoverflow.com/questions/19391149/
    numpy-mean-and-variance-from-single-function

    xs: sequence of values
    ddof: delta degrees of freedom

    returns: pair of float, mean and var
    """
    xs = np.asarray(xs)
    mean_x = xs.mean()
    s2 = var(xs, mean_x, ddof)
    return mean_x, s2


def trim(t, p=0.01):
    """Trims the largest and smallest elements of t.

    Args:
        t: sequence of numbers
        p: fraction of values to trim off each end

    Returns:
        sequence of values
    """
    n = int(p * len(t))
    t = sorted(t)[n:-n]
    return t


def trimmed_mean(t, p=0.01):
    """Computes the trimmed mean of a sequence of numbers.

    Args:
        t: sequence of numbers
        p: fraction of values to trim off each end

    Returns:
        float
    """
    t = trim(t, p)
    return mean(t)


def trimmed_mean_var(t, p=0.01):
    """Computes the trimmed mean and variance of a sequence of numbers.

    Side effect: sorts the list.

    Args:
        t: sequence of numbers
        p: fraction of values to trim off each end

    Returns:
        float
    """
    t = trim(t, p)
    mu, var_x = mean_var(t)
    return mu, var_x


def cohen_effect_size(group1, group2):
    """Compute Cohen's d.

    group1: Series or NumPy array
    group2: Series or NumPy array

    returns: float
    """
    diff = group1.mean() - group2.mean()

    n1, n2 = len(group1), len(group2)
    var1 = group1.var()
    var2 = group2.var()

    pooled_var = (n1 * var1 + n2 * var2) / (n1 + n2)
    d = diff / np.sqrt(pooled_var)
    return d


def cov(xs, ys, meanx=None, meany=None):
    """Computes Cov(X, Y).

    Args:
        xs: sequence of values
        ys: sequence of values
        meanx: optional float mean of xs
        meany: optional float mean of ys

    Returns:
        Cov(X, Y)
    """
    xs = np.asarray(xs)
    ys = np.asarray(ys)

    if meanx is None:
        meanx = np.mean(xs)
    if meany is None:
        meany = np.mean(ys)

    cov_x = np.dot(xs - meanx, ys - meany) / len(xs)
    return cov_x


def corr(xs, ys):
    """Computes Corr(X, Y).

    Args:
        xs: sequence of values
        ys: sequence of values

    Returns:
        Corr(X, Y)
    """
    xs = np.asarray(xs)
    ys = np.asarray(ys)

    meanx, varx = mean_var(xs)
    meany, vary = mean_var(ys)

    corr_x = cov(xs, ys, meanx, meany) / np.sqrt(varx * vary)

    return corr_x


def serial_corr(series, lag=1):
    """Computes the serial correlation of a series.

    series: Series
    lag: integer number of intervals to shift

    returns: float correlation
    """
    xs = series[lag:]
    ys = series.shift(lag)[lag:]
    corr_x = corr(xs, ys)
    return corr_x


def spearman_corr(xs, ys):
    """Computes Spearman's rank correlation.

    Args:
        xs: sequence of values
        ys: sequence of values

    Returns:
        float Spearman's correlation
    """
    xranks = pd.Series(xs).rank()
    yranks = pd.Series(ys).rank()
    return corr(xranks, yranks)


def map_to_ranks(t):
    """Returns a list of ranks corresponding to the elements in t.

    Args:
        t: sequence of numbers

    Returns:
        list of integer ranks, starting at 1
    """
    # pair up each value with its index
    pairs = enumerate(t)

    # sort by value
    sorted_pairs = sorted(pairs, key=itemgetter(1))

    # pair up each pair with its rank
    ranked = enumerate(sorted_pairs)

    # sort by index
    resorted = sorted(ranked, key=lambda trip: trip[1][0])

    # extract the ranks
    ranks = [trip[0] + 1 for trip in resorted]
    return ranks


def least_squares(xs, ys):
    """Computes a linear least squares fit for ys as a function of xs.

    Args:
        xs: sequence of values
        ys: sequence of values

    Returns:
        tuple of (intercept, slope)
    """
    meanx, varx = mean_var(xs)
    meany = mean(ys)

    slope = cov(xs, ys, meanx, meany) / varx
    inter = meany - slope * meanx

    return inter, slope


def fit_line(xs, inter, slope):
    """Fits a line to the given data.

    xs: sequence of x

    returns: tuple of numpy arrays (sorted xs, fit ys)
    """
    fit_xs = np.sort(xs)
    fit_ys = inter + slope * fit_xs
    return fit_xs, fit_ys


def residuals(xs, ys, inter, slope):
    """Computes residuals for a linear fit with parameters inter and slope.

    Args:
        xs: independent variable
        ys: dependent variable
        inter: float intercept
        slope: float slope

    Returns:
        list of residuals
    """
    xs = np.asarray(xs)
    ys = np.asarray(ys)
    res = ys - (inter + slope * xs)
    return res


def coef_determination(ys, res):
    """Computes the coefficient of determination (R^2) for given residuals.

    Args:
        ys: dependent variable
        res: residuals

    Returns:
        float coefficient of determination
    """
    return 1 - var(res) / var(ys)


def correlated_generator(rho):
    """Generates standard normal variates with serial correlation.

    rho: target coefficient of correlation

    Returns: iterable
    """
    x = np.random.normal(0, 1)
    yield x

    sigma = np.sqrt(1 - rho ** 2)
    while True:
        x = np.random.normal(x * rho, sigma)
        yield x


def correlated_normal_generator(mu, sigma, rho):
    """Generates normal variates with serial correlation.

    mu: mean of variate
    sigma: standard deviation of variate
    rho: target coefficient of correlation

    Returns: iterable
    """
    for x in correlated_generator(rho):
        yield x * sigma + mu


def raw_moment(xs, k):
    """Computes the kth raw moment of xs.
    """
    return sum(x ** k for x in xs) / len(xs)


def central_moment(xs, k):
    """Computes the kth central moment of xs.
    """
    mean_x = raw_moment(xs, 1)
    return sum((x - mean_x) ** k for x in xs) / len(xs)


def standardized_moment(xs, k):
    """Computes the kth standardized moment of xs.
    """
    var_x = central_moment(xs, 2)
    std_x = np.sqrt(var_x)
    return central_moment(xs, k) / std_x ** k


def skewness(xs):
    """Computes skewness.
    """
    return standardized_moment(xs, 3)


def median(xs):
    """Computes the median (50th percentile) of a sequence.

    xs: sequence or anything else that can initialize a Cdf

    returns: float
    """
    cdf = Cdf(xs)
    return cdf.value(0.5)


def iqr(xs):
    """Computes the interquartile of a sequence.

    xs: sequence or anything else that can initialize a Cdf

    returns: pair of floats
    """
    cdf = Cdf(xs)
    return cdf.value(0.25), cdf.value(0.75)


def pearson_median_skewness(xs):
    """
    Computes the Pearson median skewness.
    """
    median_x = median(xs)
    mean_x = raw_moment(xs, 1)
    var_x = central_moment(xs, 2)
    std_x = np.sqrt(var_x)
    gp = 3 * (mean_x - median_x) / std_x
    return gp


class FixedWidthVariables(object):
    """Represents a set of variables in a fixed width file."""

    def __init__(self, variables, index_base=0):
        """Initializes.

        variables: DataFrame
        index_base: are the indices 0 or 1 based?

        Attributes:
        colspecs: list of (start, end) index tuples
        names: list of string variable names
        """
        self.variables = variables

        # note: by default, subtract 1 from colspecs
        self.colspecs = variables[["start", "end"]] - index_base

        # convert colspecs to a list of pair of int
        self.colspecs = self.colspecs.astype(np.int).values.tolist()
        self.names = variables["name"]

    def read_fixed_width(self, filename, **options):
        """Reads a fixed width ASCII file.

        filename: string filename

        returns: DataFrame
        """
        df = pd.read_fwf(filename, colspecs=self.colspecs, names=self.names, **options)
        return df


def read_stata_dct(dct_file, **options):
    """Reads a Stata dictionary file.

    dct_file: string filename
    options: dict of options passed to open()

    returns: FixedWidthVariables object
    """
    type_map = dict(
        byte=int, int=int, long=int, float=float, double=float, numeric=float
    )

    var_info = []
    with open(dct_file, **options) as f:
        for line in f:
            match = re.search(r"_column\(([^)]*)\)", line)
            if not match:
                continue
            start = int(match.group(1))
            t = line.split()
            vtype, name, fstring = t[1:4]
            name = name.lower()
            if vtype.startswith("str"):
                vtype = str
            else:
                vtype = type_map[vtype]
            long_desc = " ".join(t[4:]).strip('"')
            var_info.append((start, vtype, name, fstring, long_desc))

    columns = ["start", "type", "name", "fstring", "desc"]
    variables = pd.DataFrame(var_info, columns=columns)

    # fill in the end column by shifting the start column
    variables["end"] = variables.start.shift(-1)
    variables.loc[len(variables) - 1, "end"] = 0

    dct = FixedWidthVariables(variables, index_base=1)
    return dct


def resample(xs, n=None):
    """Draw a sample from xs with the same length as xs.

    xs: sequence
    n: sample size (default: len(xs))

    returns: NumPy array
    """
    if n is None:
        n = len(xs)
    return np.random.choice(xs, n, replace=True)


def sample_rows(df, nrows, replace=False):
    """Choose a sample of rows from a DataFrame.

    df: DataFrame
    nrows: number of rows
    replace: whether to sample with replacement

    returns: DataDf
    """
    indices = np.random.choice(df.index, nrows, replace=replace)
    sample = df.loc[indices]
    return sample


def resample_rows(df):
    """Resamples rows from a DataFrame.

    df: DataFrame

    returns: DataFrame
    """
    return sample_rows(df, len(df), replace=True)


def resample_rows_weighted(df, column="finalwgt"):
    """Resamples a DataFrame using probabilities proportional to given column.

    df: DataFrame
    column: string column name to use as weights

    returns: DataFrame
    """
    weights = df[column].copy()
    weights /= sum(weights)
    indices = np.random.choice(df.index, len(df), replace=True, p=weights)
    sample = df.loc[indices]
    return sample


def percentile_row(array, p):
    """Selects the row from a sorted array that maps to percentile p.

    p: float 0--100

    returns: NumPy array (one row)
    """
    rows, _ = array.shape
    index = int(rows * p / 100)
    return array[
        index,
    ]


def percentile_rows(ys_seq, percents):
    """Given a collection of lines, selects percentiles along vertical axis.

    For example, if ys_seq contains simulation results like ys as a
    function of time, and percents contains (5, 95), the result would
    be a 90% CI for each vertical slice of the simulation results.

    ys_seq: sequence of lines (y values)
    percents: list of percentiles (0-100) to select

    returns: list of NumPy arrays, one for each percentile
    """
    nrows = len(ys_seq)
    ncols = len(ys_seq[0])
    array = np.zeros((nrows, ncols))

    for i, ys in enumerate(ys_seq):
        array[i, ] = ys

    array = np.sort(array, axis=0)

    rows = [percentile_row(array, p) for p in percents]
    return rows


def smooth(xs, sigma=2, **options):
    """Smooths a NumPy array with a Gaussian filter.

    xs: sequence
    sigma: standard deviation of the filter
    """
    return ndimage.filters.gaussian_filter1d(xs, sigma, **options)


class HypothesisTest(object):
    """Represents a hypothesis test."""

    def __init__(self, data):
        """Initializes.

        data: data in whatever form is relevant
        """
        self.data = data
        self.make_model()
        self.actual = self.test_statistic(data)
        self.test_stats = None
        self.test_cdf = None

    def p_value(self, iters=1000):
        """Computes the distribution of the test statistic and p-value.

        iters: number of iterations

        returns: float p-value
        """
        self.test_stats = [self.test_statistic(self.run_model()) for _ in range(iters)]
        self.test_cdf = Cdf(self.test_stats)

        count = sum(1 for x in self.test_stats if x >= self.actual)
        return count / iters

    def max_test_stat(self):
        """Returns the largest test statistic seen during simulations.
        """
        return max(self.test_stats)

    def test_statistic(self, data):
        """Computes the test statistic.

        data: data in whatever form is relevant
        """
        raise UnimplementedMethodException()

    def make_model(self):
        """Build a model of the null hypothesis.
        """
        raise UnimplementedMethodException()

    def run_model(self):
        """Run the model of the null hypothesis.

        returns: simulated data
        """
        raise UnimplementedMethodException()


def main():
    print("Usage: import thinkbayes")


if __name__ == "__main__":
    main()
