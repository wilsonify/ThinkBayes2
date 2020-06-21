"""
What is a distribution?
An object-oriented exploration of one of the most useful concepts in statistics.
Copyright 2016 Allen Downey
MIT License: http://opensource.org/licenses/MIT

Summary

Abstractly, a distribution is an entity that can answer questions
about the outcomes of random variables and their probabilities.
There are many ways to represent a distribution;
each representation is equivalent to the others in the sense that they contain the same information,
and you can convert from any of them to the others.

Some representations make it easy and efficient to answer some questions,
but none of the representations is best for all of the questions.
In my implementation, a `Dist` object has the attributes and methods of all representations.
From a software engineering point of view, that might not be the best design,
but it is meant to illustrate what it means to be a distribution.
In short, if you give me any representation of a distribution,
you have told me everything I need to know to answer questions about the possible outcomes and their probabilities.
Converting from one representation to another is mostly a matter of convenience and computational efficiency.
Conversely, if you are trying to find the distribution of a random variable,
you can do it by computing whichever representation is easiest to figure out.
"""

from inspect import getsourcelines

import matplotlib.pyplot as plt
import numpy as np
import pytest
from numpy.fft import fft, ifft
from thinkbayes import Pmf, Cdf
from thinkbayes import thinkplot


def show_code(func):
    lines, _ = getsourcelines(func)
    for line in lines:
        print(line, end="")


def iqr(cdf):
    values = cdf.values((0.25, 0.75))
    return np.diff(values)[0]


def find_min(cdf, k):
    return Cdf(cdf.xs, 1 - (1 - cdf.ps) ** k)


class CharFunc:
    r"""
    Characteristic function
    At this point we've answered all the questions on the list, but I want to come back to addition,
    because the algorithm we used with the `Pmf` representation is not as efficient as it could be.
    It enumerates all pairs of outcomes, so if there are $n$ values in each `Pmf`, the run time is $O(n^2)$.
    We can do better.
    The key is the **characteristic function**, which is the Fourier transform (FT) of the PMF.
    If you are familiar with the Fourier transform and the Convolution Theorem, keep reading.
    Otherwise, skip the rest of this cell and get to the code, which is much simpler than the explanation.

    ### Details for people who know about convolution
    If you are familiar with the FT in the context of spectral analysis of signals,
    you might wonder why we would possibly want to compute the FT of a PMF.
    The reason is the Convolution Theorem.
    It turns out that the algorithm we used to "add" two `Pmf` objects is a form of convolution.
    To see how that works, suppose we are computing the distribution of $Z = X+Y$.
    To make things concrete, let's compute the probability that the sum, $Z$ is 5.
    To do that, we can enumerate all possible values of $X$ like this:
    $Prob(Z=5) = \sum_x Prob(X=x) \cdot Prob(Y=5-x)$
    Now we can write each of those probabilities in terms of the PMF of $X$, $Y$, and $Z$:
    $PMF_Z(5) = \sum_x PMF_X(x) \cdot PMF_Y(5-x)$
    And now we can generalize by replacing 5 with any value of $z$:
    $PMF_Z(z) = \sum_x PMF_X(x) \cdot PMF_Y(z-x)$
    You might recognize that computation as convolution, denoted with the operator $\ast$.
    $PMF_Z = PMF_X \ast PMF_Y$
    Now, according to the Convolution Theorem:
    $FT(PMF_X \ast Y) = FT(PMF_X) \cdot FT(PMF_Y)$
    Or, taking the inverse FT of both sides:
    $PMF_X \ast PMF_Y = IFT(FT(PMF_X) \cdot FT(PMF_Y))$
    In words, to compute the convolution of $PMF_X$ and $PMF_Y$,
    we can compute the FT of $PMF_X$ and $PMF_Y$ and multiply them together, then compute the inverse FT of the result.
    Let's see how that works.  Here's a class that represents a characteristic function.
    """

    def __init__(self, hs):
        """Initializes the CF.

        hs: NumPy array of complex
        """
        self.hs = hs

    def __mul__(self, other):
        """Computes the elementwise product of two CFs."""
        return CharFunc(self.hs * other.hs)

    def make_pmf(self, thresh=1e-11):
        """Converts a CF to a PMF.

        Values with probabilities below `thresh` are dropped.
        """
        ps = ifft(self.hs)
        d = dict((i, p) for i, p in enumerate(ps.real) if p > thresh)
        return Pmf(d)

    def plot_cf(self, **options):
        """Plots the real and imaginary parts of the CF."""
        n = len(self.hs)
        xs = np.arange(-n // 2, n // 2)
        hs = np.roll(self.hs, len(self.hs) // 2)
        plt.plot(xs, hs.real, label="real", **options)
        plt.plot(xs, hs.imag, label="imag", **options)
        plt.legend()


def compute_fft(d, n=256):
    """
    The attribute, `hs`, is the Fourier transform of the `Pmf`, represented as a NumPy array of complex numbers.
    The following function takes a dictionary that maps outcomes to their probabilities,
    and computes the FT of the PDF:

    Computes the FFT of a PMF of integers.

    Values must be integers less than `n`.
    """
    xs, freqs = zip(*d.items())
    ps = np.zeros(n)
    ps[xs,] = freqs
    hs = fft(ps)
    return hs


class Dist(Pmf, Cdf, CharFunc):
    """
    Distributions
    Finally, let's back to the question we started with: *what is a distribution?*
    I've said that `Pmf`, `Cdf`, and `CharFunc` are different ways to represent the same information.
    For the questions we want to answer, some representations are better than others.
    But how should we represent the distribution itself?
    One option is to treat each representation as a **mixin**; that is, a class that provides a set of capabilities.
    A distribution inherits all of the capabilities from all of the representations.
    Here's a class that shows what I mean:
    """

    def __init__(self, d):
        """Initializes the Dist.

        Calls all three __init__ methods.
        """
        Pmf.__init__(self, d)
        Cdf.__init__(self, d)
        CharFunc.__init__(self, compute_fft(d))

    def __add__(self, other):
        """Computes the distribution of the sum using Pmf.__add__.
        """
        pmf = Pmf.__add__(self, other)
        return Dist(pmf.d)

    def __mul__(self, other):
        """Computes the distribution of the sum using CharFunc.__mul__.
        """
        pmf = CharFunc.__mul__(self, other).make_pmf()
        return Dist(pmf.d)


def test_print(d6):
    """
    Initially the "probabilities" are all 1,
    so the total probability in the `Pmf` is 6,
    which doesn't make a lot of sense.
    """
    d6.print_size()


def test_norm(d6):
    """
    In a proper, meaningful, PMF, the probabilities add up to 1,
    which implies that one outcome, and only one outcome, will occur (for any given roll of the die).
    We can take this "unnormalized" distribution and make it a proper `Pmf` using the `Normalize` method.
    Here's what the method looks like:

    `Normalize` adds up the probabilities in the PMF and divides through by the total.
    The result is a `Pmf` with probabilities that add to 1.
    """

    d6.print_size()


def test_3(d6):
    """
    The fundamental operation provided by a `Pmf` is a "lookup";
    that is, we can look up an outcome and get the corresponding probability.
    `Pmf` provides `__getitem__`, so we can use bracket notation to look up an outcome:
    :param d6:
    :return:
    """
    assert d6[3] == 1 / 6


def test_7(d6):
    """
    And if you look up a value that's not in the `Pmf`, the probability is 0.
    :return:
    """
    assert d6[7] == 0


def test_exercise(d6):
    # **Exerise:** Create a `Pmf` that represents a six-sided die that is red on two sides and blue on the other four.

    # Solution

    die = Pmf(dict(red=2, blue=4))
    die.normalize()
    die.print()


def test_moments_expecations(d6):
    """
    Is that all there is?
    So is a `Pmf` a distribution?
    No.  At least in this framework, a `Pmf` is one of several representations of a distribution.
    Other representations include the **cumulative distribution function**, or CDF, and the **characteristic function**.
    These representations are equivalent in the sense that they all contain the same informaton;
    if I give you any one of them, you can figure out the others (and we'll see how soon).

    So why would we want different representations of the same information?
    The fundamental reason is that there are many different operations we would like to perform with distributions;
    that is, questions we would like to answer.
    Some representations are better for some operations, but none of them is the best for all operations.

    So what are the questions we would like a distribution to answer?  They include:

    *  What is the probability of a given outcome?
    *  What is the mean of the outcomes, taking into account their probabilities?
    *  What is the variance, and other moments, of the outcome?
    *  What is the probability that the outcome exceeds (or falls below) a threshold?
    *  What is the median of the outcomes, that is, the 50th percentile?
    *  What are the other percentiles?
    *  How can get generate a random sample from this distribution, with the appropriate probabilities?
    *  If we run two random processes and choose the maximum of the outcomes (or minimum),
    what is the distribution of the result?
    *  If we run two random processes and add up the results, what is the distribution of the sum?

    Each of these questions corresponds to a method we would like a distribution to provide.
    But as I said, there is no one representation that answers all of them easily and efficiently.
    So let's look at the different representations and see what they can do.

    Getting back to the `Pmf`, we've already seen how to look up the probability of a given outcome.

    Python dictionaries are implemented using hash tables, so we expect `__getitem__` to be fast.
    In terms of algorithmic complexity, it is constant time, or $O(1)$.

    Moments and expecations

    The `Pmf` representation is also good for computing mean, variance, and other moments.
    This implementation is efficient,
    in the sense that it is $O(n)$, and because it uses a comprehension to traverse the outcomes, the overhead is low.
    The implementation of `Pmf.Var` is similar
    And here's how they are used:
    The structure of `Mean` and `Var` is the same:
    they traverse the outcomes and their probabilities, `x` and `p`,
    and add up the product of `p` and some function of `x`.

    """
    d6.mean(), d6.var()


def test_third_central_moment(d6):
    r"""
    We can generalize this structure to compute the **expectation** of any function of `x`, which is defined as
    $E[f] = \sum_x p(x) f(x)$

    `Pmf` provides `Expect`, which takes a function object, `func`, and returns the expectation of `func`:
    As an example, we can use `Expect` to compute the third central moment of the distribution:
    Because the distribution is symmetric, the third central moment is 0.
    """
    mu = d6.mean()
    result = d6.expect(lambda x: (x - mu) ** 3)
    assert result == 0


def test_addition(d6):
    """
    The next question we'll answer is the last one on the list:
    if we run two random processes and add up the results,
    what is the distribution of the sum?
    In other words, if the result of the first process is a random variable, $X$, and the result of the second is $Y$,
    what is the distribution of $X+Y$?

    The `Pmf` representation of the distribution can answer this question pretty well,
    but we'll see later that the characteristic function is even better.

    The outer loop traverses the outcomes and probabilities of the first `Pmf`;
    the inner loop traverses the second `Pmf`.
    Each time through the loop, we compute the sum of the outcome pair, `v1` and `v2`,
      and the probability that the pair occurs.

    Note that this method implicitly assumes that the two processes are independent;
    that is, the outcome from one does not affect the other.
    That's why we can compute the probability of the pair by multiplying the probabilities of the outcomes.

    To demonstrate this method, we'll start with `d6` again.
    When we use the `+` operator, Python invokes `__add__`, which invokes `AddPmf`, which returns a new `Pmf` object.
    Here's the `Pmf` that represents the sum of two dice:
    """

    thinkplot.plot_pdf_line(d6)
    twice = d6 + d6
    thinkplot.plot_pdf_line(twice, color="green")


def test_threedice(d6):
    """
    # And here's the `Pmf` that represents the sum of three dice.
    # As we add up more dice, the result converges to the bell shape of the Gaussian distribution.
    """
    twice = d6 + d6
    thrice = twice + d6
    thinkplot.plot_pdf_line(d6)
    thinkplot.plot_pdf_line(twice, color="green")
    thinkplot.plot_pdf_line(thrice, color="red")


def test_ex(d6):
    """
    **Exercise:**
    you have a `Pmf` that represents a die with red on 2 sides and blue on the other 4.
    Use the `+` operator to compute the outcomes of rolling two of these dice and the probabilities of the outcomes.

    Note: if you represent the outcomes as strings, `AddPmf` concatenates them instead of adding, which actually works.
    """
    die = Pmf(dict(red=2, blue=4))
    dice = die + die
    dice.print_size()


def test_cumulative_probabilities(d6):
    r"""
    Cumulative probabilities
    The next few questions on the list are related to the median and other percentiles.
    They are harder to answer with the `Pmf` representation,
    but easier with a **cumulative distribution function** (CDF).

    A CDF is a map from an outcome, $x$, to its cumulative probability,
    which is the probability that the outcome is less than or equal to $x$.  In math notation:
    $CDF(x) = Prob(X \le x)$
    where $X$ is the outcome of a random process, and $x$ is the threshold we are interested in.
    For example, if $CDF$ is the cumulative distribution for the sum of three dice,
    the probability of getting 5 or less is $CDF(5)$, and the probability of getting 6 or more is $1 - CDF(5)$.
    `thinkbayes` provides a class called Cdf that represents a cumulative distribution function.
    It uses a sorted list of outcomes and the corresponding list of cumulative probabilities.
    The `__init__` method is complicated because it accepts a lot of different parameters.
    The important part is the last 4 lines.

    `xs` is the sorted list of values, and `freqs` are their frequencies or probabilities.
    `ps` is the list of cumulative frequencies or probabilities,
    which we normalize by dividing through by the last element.
    Here's how we use it to create a `Cdf` object for the sum of three dice:
    Because we have to sort the values, the time to compute a `Cdf` is $O(n \log n)$.

    Here's what the CDF looks like:
    The range of the CDF is always from 0 to 1.
    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)
    cdf.print()
    thinkplot.plot_cdf_line(cdf)


def test_cdf5(d6):
    r"""
    Now we can compute $CDF(x)$ by searching the `xs` to find the right location, or index,
    and then looking up the corresponding probability.
    Because the `xs` are sorted, we can use bisection search, which is $O(\log n)$.

    `Cdf` provides `Probs`, which takes an array of values and returns the corresponding probabilities:

    The details here are a little tricky because we have to deal with some "off by one" problems,
    and if any of the values are less than the smallest value in the `Cdf`,
    we have to handle that as a special case.
    But the basic idea is simple, and the implementation is efficient.

    Now we can look up probabilities for a sequence of values:
    `Cdf` also provides `__getitem__`, so we can use brackets to look up a single value:
    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)
    cdf.probs((2, 10, 18))
    assert cdf[5] == pytest.approx(0.05, abs=0.01)


def test_ex15(d6):
    """
    **Exercise:**
    If you roll three dice, what is the probability of getting 15 or more?
    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)
    assert 1 - cdf[14] == pytest.approx(0.0926, abs=0.001)


def test_reverse_lookup(d6):
    r"""
    Reverse lookup
    You might wonder why I represent a `Cdf` with two lists rather than a dictionary.
    After all, a dictionary lookup is constant time and bisection search is logarithmic.
    The reason is that we often want to use a `Cdf` to do a reverse lookup;
    that is, given a probability, we would like to find the corresponding value.
    With two sorted lists, a reverse lookup has the same performance as a forward loopup, $O(\log n)$.

    And here's an example that finds the 10th, 50th, and 90th percentiles:
    The `Cdf` representation is also good at generating random samples,
    by choosing a probability uniformly from 0 to 1 and finding the corresponding value.

    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)

    cdf.values((0.1, 0.5, 0.9))


def test_sample1(d6):
    r"""
    The result is a NumPy array with the given `shape`.
    The time to generate each random choice is $O(\log n)$
    Here are some examples that use it.
    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)
    cdf.sample(1)
    cdf.sample(6)
    cdf.sample((2, 2))


def test_ex_irq(d6):
    """
    **Exercise:**
    Write a function that takes a `Cdf` object and returns the interquartile range (IQR),
    which is the difference between the 75th and 25th percentiles.
    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)
    iqr(cdf)


def test_max_min(d6):
    """
    Max and min
    The `Cdf` representation is particularly good for finding the distribution of a maximum.
    For example, in Dungeons and Dragons,
    players create characters with random properties like strength and intelligence.
    The properties are generated by rolling three dice and adding them,
    so the CDF for each property is the `Cdf` we used in this example.
    Each character has 6 properties, so we might wonder what the distribution is for the best of the six.

    To get the distribution of the maximum, we make a new `Cdf` with the same values as the original,
    and with the `ps` raised to the `k`th power.  Simple, right?

    To see how it works, suppose you generate six properties and your best is only a 10.
    That's unlucky, but you might wonder how unlucky.
    So, what is the chance of rolling 3 dice six times, and never getting anything better than 10?

    Well, that means that all six values were 10 or less.
    The probability that each of them is 10 or less is $CDF(10)$,
    because that's what the CDF means.
    So the probability that all 6 are 10 or less is $CDF(10)^6$.

    Now we can generalize that by replacing $10$ with any value of $x$ and $6$ with any integer $k$.
    The result is $CDF(x)^k$,
    which is the probability that all $k$ rolls are $x$ or less, and that is the CDF of the maximum.
    So the chance of generating a character whose best property is 10 is less than 2%.
    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)
    best = cdf.max(6)
    thinkplot.plot_cdf_line(best)
    assert best[10] == pytest.approx(0.0156, abs=0.001)


def test_ex_min(d6):
    """
    # **Exercise:**  Write a function that takes a CDF and returns the CDF of the *minimum* of `k` values.
    #
    # Hint: If the minimum is less than $x$, that means all `k` values must be less than $x$.
    """
    twice = d6 + d6
    thrice = twice + d6
    cdf = Cdf(thrice)
    worst = find_min(cdf, 6)
    thinkplot.plot_cdf_line(worst)


def test_fft(d6):
    r"""
    `fft` computes the Fast Fourier Transform (FFT), which is called "fast" because the run time is $O(n \log n)$.

    Here's what the characteristic function looks like for the sum of three dice
    (plotting the real and imaginary parts of `hs`):
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    cf.plot_cf()


def test_characteristic(d6):
    """
    The characteristic function contains all of the information from the `Pmf`,
    but it is encoded in a form that is hard to interpret.
    However, if we are given a characteristic function, we can find the corresponding `Pmf`.

    `CharFunc` provides `make_pmf`, which uses the inverse FFT to get back to the `Pmf` representation.


    Now we can use the characteristic function to compute a convolution.
    `CharFunc` provides `__mul__`, which multiplies the `hs` elementwise and returns a new `CharFunc` object:
    And here's how we can use it to compute the distribution of the sum of 6 dice.
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    thinkplot.plot_pdf_line(cf.make_pmf())
    sixth = (cf * cf).make_pmf()
    thinkplot.plot_pdf_line(sixth)


def test_sixth(d6):
    r"""
    Here are the probabilities, mean, and variance.
    This might seem like a roundabout way to compute a convolution, but it is efficient.
    The time to Compute the `CharFunc` objects is $O(n \log n)$.
    Multiplying them together is $O(n)$.  And converting back to a `Pmf` is $O(n \log n)$.

    So the whole process is $O(n \log n)$, which is better than `Pmf.__add__`, which is $O(n^2)$.
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    sixth.print()
    sixth.mean(), sixth.var()


def test_mags(d6):
    """
    **Exercise:**
    Plot the magnitude of `cf.hs` using `np.abs`.  What does that shape look like?
    Hint: it might be clearer if you us `np.roll` to put the peak of the CF in the middle.
    The result approximates a Gaussian curve because
    the PMF is approximately Gaussian and the FT of a
    Gaussian is also Gaussian
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    n = len(cf.hs)
    mags = np.abs(cf.hs)
    plt.plot(np.roll(mags, n // 2))


def test_dist(d6):
    """
    When you create a `Dist`, you provide a dictionary of values and probabilities.
    `Dist.__init__` calls the other three `__init__` methods
    to create the `Pmf`, `Cdf`, and `CharFunc` representations.
    The result is an object that has all the attributes and methods of the three representations.
    As an example, I'll create a `Dist` that represents the sum of six dice:
    We inherit `__getitem__` from `Pmf`, so we can look up the probability of a value.
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    dist = Dist(sixth.d)
    thinkplot.plot_pdf_line(dist)
    assert dist[21] == pytest.approx(0.0928, abs=0.01)


def test_meanvar(d6):
    """
    We also get mean and variance from `Pmf`:
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    dist = Dist(sixth.d)
    dist.mean(), dist.var()


def test_value_array(d6):
    """
    But we can also use methods from `Cdf`, like `ValueArray`:
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    dist = Dist(sixth.d)
    dist.ValueArray((0.25, 0.5, 0.75))


def test_probs(d6):
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    dist = Dist(sixth.d)
    dist.probs((18, 21, 24))


def test_sample(d6):
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    dist = Dist(sixth.d)
    dist.sample(10)
    thinkplot.plot_cdf_line(dist.max(6))


def test_slow(d6):
    """
    `Dist.__add__` uses `Pmf.__add__`, which performs convolution the slow way:
        # Either way, we get the answer, which is 42.
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    dist = Dist(sixth.d)
    twelfth = dist + dist
    thinkplot.plot_pdf_line(twelfth)
    twelfth.mean()


def test_mul(d6):
    """
    `Dist.__mul__` uses `CharFunc.__mul__`, which performs convolution the fast way.
        # Either way, we get the answer, which is 42.
    """
    twice = d6 + d6
    thrice = twice + d6
    hs = compute_fft(thrice.d)
    cf = CharFunc(hs)
    sixth = (cf * cf).make_pmf()
    dist = Dist(sixth.d)
    twelfth_fft = dist * dist
    thinkplot.plot_pdf_line(twelfth_fft)
    twelfth_fft.mean()
