"""
Bayesian Statistics Made Simple
Code and exercises from my workshop on Bayesian statistics in Python.
Copyright 2016 Allen Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf, Suite
from thinkbayes import thinkplot


def test_pmfs():
    # Working with Pmfs
    # Create a Pmf object to represent a six-sided die.

    d6 = Pmf()

    # A Pmf is a map from possible outcomes to their probabilities.

    for x in [1, 2, 3, 4, 5, 6]:
        d6[x] = 1

    # Initially the probabilities don't add up to 1.

    d6.Print()

    # `Normalize` adds up the probabilities and divides through.  The return value is the total probability before normalizing.

    d6.Normalize()

    # Now the Pmf is normalized.

    d6.Print()

    # And we can compute its mean (which only works if it's normalized).

    d6.Mean()

    # `Random` chooses a random value from the Pmf.

    d6.Random()

    # `thinkplot` provides methods for plotting Pmfs in a few different styles.

    thinkplot.Hist(d6)

    # **Exercise 1:**  The Pmf object provides `__add__`, so you can use the `+` operator to compute the Pmf of the sum of two dice.
    #
    # Compute and plot the Pmf of the sum of two 6-sided dice.

    # Solution
    thinkplot.Hist(d6 + d6)

    # **Exercise 2:** Suppose I roll two dice and tell you the result is greater than 3.
    #
    # Plot the Pmf of the remaining possible outcomes and compute its mean.

    # Solution

    pmf = d6 + d6
    pmf[2] = 0
    pmf[3] = 0
    pmf.Normalize()
    thinkplot.Hist(pmf)
    pmf.Mean()

    # The cookie problem

    # Create a Pmf with two equally likely hypotheses.
    #

    cookie = Pmf(["Bowl1", "Bowl2"])
    cookie.Print()

    # Update each hypothesis with the likelihood of the data (a vanilla cookie).

    cookie["Bowl1"] *= 0.75
    cookie["Bowl2"] *= 0.5
    cookie.Normalize()

    # Print the posterior probabilities.

    cookie.Print()

    # **Exercise 3:** Suppose we put the first cookie back, stir, choose again from the same bowl, and get a chocolate cookie.
    #
    # Hint: The posterior (after the first cookie) becomes the prior (before the second cookie).

    # Solution

    cookie["Bowl1"] *= 0.25
    cookie["Bowl2"] *= 0.5
    cookie.Normalize()
    cookie.Print()

    # **Exercise 4:** Instead of doing two updates, what if we collapse the two pieces of data into one update?
    #
    # Re-initialize `Pmf` with two equally likely hypotheses and perform one update based on two pieces of data, a vanilla cookie and a chocolate cookie.
    #
    # The result should be the same regardless of how many updates you do (or the order of updates).

    # Solution

    cookie = Pmf(["Bowl1", "Bowl2"])
    cookie["Bowl1"] *= 0.75 * 0.25
    cookie["Bowl2"] *= 0.5 * 0.5
    cookie.Normalize()
    cookie.Print()

    # The dice problem

    # Create a Suite to represent dice with different numbers of sides.

    pmf = Pmf([4, 6, 8, 12])
    pmf.Print()

    # **Exercise 5:** We'll solve this problem two ways.  First we'll do it "by hand", as we did with the cookie problem; that is, we'll multiply each hypothesis by the likelihood of the data, and then renormalize.
    #
    # In the space below, update `suite` based on the likelihood of the data (rolling a 6), then normalize and print the results.

    # Solution

    pmf[4] *= 0
    pmf[6] *= 1 / 6
    pmf[8] *= 1 / 8
    pmf[12] *= 1 / 12

    pmf.Normalize()
    pmf.Print()

    # **Exercise 6:**  Now let's do the same calculation using `Suite.Update`.
    #
    # Write a definition for a new class called `Dice` that extends `Suite`.  Then define a method called `Likelihood` that takes `data` and `hypo` and returns the probability of the data (the outcome of rolling the die) for a given hypothesis (number of sides on the die).
    #
    # Hint: What should you do if the outcome exceeds the hypothetical number of sides on the die?
    #
    # Here's an outline to get you started:

    class Dice(Suite):
        # hypo is the number of sides on the die
        # data is the outcome
        def Likelihood(self, data, hypo):
            return 1

    # Solution

    class Dice(Suite):
        # hypo is the number of sides on the die
        # data is the outcome
        def Likelihood(self, data, hypo):
            if data > hypo:
                return 0
            else:
                return 1 / hypo

    # Now we can create a `Dice` object and update it.

    dice = Dice([4, 6, 8, 12])
    dice.Update(6)
    dice.Print()

    # If we get more data, we can perform more updates.

    for roll in [8, 7, 7, 5, 4]:
        dice.Update(roll)

    # Here are the results.

    dice.Print()

    # The German tank problem

    # The German tank problem is actually identical to the dice problem.

    class Tank(Suite):
        # hypo is the number of tanks
        # data is an observed serial number
        def Likelihood(self, data, hypo):
            if data > hypo:
                return 0
            else:
                return 1 / hypo

    # Here are the posterior probabilities after seeing Tank #37.

    tank = Tank(range(100))
    tank.Update(37)
    thinkplot.Pdf(tank)
    tank.Mean()

    # **Exercise 7:**  Suppose we see another tank with serial number 17.  What effect does this have on the posterior probabilities?
    #
    # Update the suite again with the new data and plot the results.

    # Solution

    thinkplot.Pdf(tank, color="0.7")
    tank.Update(17)
    thinkplot.Pdf(tank)
    tank.Mean()

    # The Euro problem

    #
    # **Exercise 8:**  Write a class definition for `Euro`, which extends `Suite` and defines a likelihood function that computes the probability of the data (heads or tails) for a given value of `x` (the probability of heads).
    #
    # Note that `hypo` is in the range 0 to 100.  Here's an outline to get you started.

    class Euro(Suite):
        def Likelihood(self, data, hypo):
            """
            hypo is the prob of heads (0-100)
            data is a string, either 'H' or 'T'
            """
            return 1

    # Solution

    class Euro(Suite):
        def Likelihood(self, data, hypo):
            """
            hypo is the prob of heads (0-100)
            data is a string, either 'H' or 'T'
            """
            x = hypo / 100
            if data == "H":
                return x
            else:
                return 1 - x

    # We'll start with a uniform distribution from 0 to 100.

    euro = Euro(range(101))
    thinkplot.Pdf(euro)

    # Now we can update with a single heads:

    euro.Update("H")
    thinkplot.Pdf(euro)

    # Another heads:

    euro.Update("H")
    thinkplot.Pdf(euro)

    # And a tails:

    euro.Update("T")
    thinkplot.Pdf(euro)

    # Starting over, here's what it looks like after 7 heads and 3 tails.

    euro = Euro(range(101))

    for outcome in "HHHHHHHTTT":
        euro.Update(outcome)

    thinkplot.Pdf(euro)
    euro.MaximumLikelihood()

    # The maximum posterior probability is 70%, which is the observed proportion.
    #
    # Here are the posterior probabilities after 140 heads and 110 tails.

    euro = Euro(range(101))

    evidence = "H" * 140 + "T" * 110
    for outcome in evidence:
        euro.Update(outcome)

    thinkplot.Pdf(euro)

    # The posterior mean s about 56%

    euro.Mean()

    # So is the value with Maximum Aposteriori Probability (MAP).

    euro.MAP()

    # The posterior credible interval has a 90% chance of containing the true value (provided that the prior distribution truly represents our background knowledge).

    euro.CredibleInterval(90)

    # ## Swamping the prior
    #
    # The following function makes a Euro object with a triangle prior.

    def TrianglePrior():
        """Makes a Suite with a triangular prior."""
        suite = Euro(label="triangle")
        for x in range(0, 51):
            suite[x] = x
        for x in range(51, 101):
            suite[x] = 100 - x
        suite.Normalize()
        return suite

    # And here's what it looks like:

    euro1 = Euro(range(101), label="uniform")
    euro2 = TrianglePrior()
    thinkplot.Pdfs([euro1, euro2])
    thinkplot.Config(title="Priors")

    # **Exercise 9:** Update euro1 and euro2 with the same data we used before (140 heads and 110 tails) and plot the posteriors.  How big is the difference in the means?

    # Solution

    evidence = "H" * 140 + "T" * 110
    for outcome in evidence:
        euro1.Update(outcome)
        euro2.Update(outcome)

    thinkplot.Pdfs([euro1, euro2])
    thinkplot.Config(title="Posteriors")
    euro1.Mean(), euro2.Mean()
