"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf, Suite


def test_pmf_class():
    # ## The Pmf class
    #
    # I'll start by making a Pmf that represents the outcome of a six-sided die.  Initially there are 6 values with equal probability.

    pmf = Pmf()
    for x in [1, 2, 3, 4, 5, 6]:
        pmf[x] = 1

    pmf.Print()

    # To be true probabilities, they have to add up to 1.  So we can normalize the Pmf:

    pmf.Normalize()

    # The return value from `Normalize` is the sum of the probabilities before normalizing.

    pmf.Print()

    # A faster way to make a Pmf is to provide a sequence of values.  The constructor adds the values to the Pmf and then normalizes:

    pmf = Pmf([1, 2, 3, 4, 5, 6])
    pmf.Print()

    # To extract a value from a Pmf, you can use `Prob`

    pmf.Prob(1)

    # Or you can use the bracket operator.

    print(pmf[1])

    #   Either way, if you ask for the probability of something that's not in the Pmf, the result is 0.

    print(pmf[7])

    # ## The cookie problem
    #
    # Here's a Pmf that represents the prior distribution.

    pmf = Pmf()
    pmf["Bowl1"] = 0.5
    pmf["Bowl2"] = 0.5
    pmf.Print()

    # And we can update it using `Mult`

    pmf.Mult("Bowl1", 0.75)
    pmf.Mult("Bowl2", 0.5)
    pmf.Print()

    # Or here's the shorter way to construct the prior.

    pmf = Pmf(["Bowl1", "Bowl2"])
    pmf.Print()

    # And we can use `*=` for the update.

    pmf["Bowl1"] *= 0.75
    pmf["Bowl2"] *= 0.5
    pmf.Print()

    # Either way, we have to normalize the posterior distribution.

    pmf.Normalize()
    pmf.Print()

    # ## The Bayesian framework
    #
    # Here's the same computation encapsulated in a class.

    class Cookie(Pmf):
        """A map from string bowl ID to probablity."""

        def __init__(self, hypos):
            """Initialize self.

            hypos: sequence of string bowl IDs
            """
            Pmf.__init__(self)
            for hypo in hypos:
                self.Set(hypo, 1)
            self.Normalize()

        def Update(self, data):
            """Updates the PMF with new data.

            data: string cookie type
            """
            for hypo in self.Values():
                self[hypo] *= self.Likelihood(data, hypo)
            self.Normalize()

        mixes = {
            "Bowl1": dict(vanilla=0.75, chocolate=0.25),
            "Bowl2": dict(vanilla=0.5, chocolate=0.5),
        }

        def Likelihood(self, data, hypo):
            """The likelihood of the data under the hypothesis.

            data: string cookie type
            hypo: string bowl ID
            """
            mix = self.mixes[hypo]
            like = mix[data]
            return like

    # We can confirm that we get the same result.

    pmf = Cookie(["Bowl1", "Bowl2"])
    pmf.Update("vanilla")
    pmf.Print()

    # But this implementation is more general; it can handle any sequence of data.

    dataset = ["vanilla", "chocolate", "vanilla"]
    for data in dataset:
        pmf.Update(data)

    pmf.Print()

    # ## The Monty Hall problem
    #
    # The Monty Hall problem might be the most contentious question in
    # the history of probability.  The scenario is simple, but the correct
    # answer is so counterintuitive that many people just can't accept
    # it, and many smart people have embarrassed themselves not just by
    # getting it wrong but by arguing the wrong side, aggressively,
    # in public.
    #
    # Monty Hall was the original host of the game show *Let's Make a
    # Deal*.  The Monty Hall problem is based on one of the regular
    # games on the show.  If you are on the show, here's what happens:
    #
    # *  Monty shows you three closed doors and tells you that there is a
    #    prize behind each door: one prize is a car, the other two are less
    #    valuable prizes like peanut butter and fake finger nails.  The
    #    prizes are arranged at random.
    #
    # *  The object of the game is to guess which door has the car.  If
    #    you guess right, you get to keep the car.
    #
    # *  You pick a door, which we will call Door A.  We'll call the
    #    other doors B and C.
    #
    # *  Before opening the door you chose, Monty increases the
    #    suspense by opening either Door B or C, whichever does not
    #    have the car.  (If the car is actually behind Door A, Monty can
    #    safely open B or C, so he chooses one at random.)
    #
    # *  Then Monty offers you the option to stick with your original
    #    choice or switch to the one remaining unopened door.
    #
    # The question is, should you "stick" or "switch" or does it
    # make no difference?
    #
    # Most people have the strong intuition that it makes no difference.
    # There are two doors left, they reason, so the chance that the car
    # is behind Door A is 50%.
    #
    # But that is wrong.  In fact, the chance of winning if you stick
    # with Door A is only 1/3; if you switch, your chances are 2/3.
    #
    # Here's a class that solves the Monty Hall problem.

    class Monty(Pmf):
        """Map from string location of car to probability"""

        def __init__(self, hypos):
            """Initialize the distribution.

            hypos: sequence of hypotheses
            """
            Pmf.__init__(self)
            for hypo in hypos:
                self.Set(hypo, 1)
            self.Normalize()

        def Update(self, data):
            """Updates each hypothesis based on the data.

            data: string 'A', 'B', or 'C'
            """
            for hypo in self.Values():
                self[hypo] *= self.Likelihood(data, hypo)
            self.Normalize()

        def Likelihood(self, data, hypo):
            """Compute the likelihood of the data under the hypothesis.

            hypo: string name of the door where the prize is
            data: string name of the door Monty opened
            """
            if hypo == data:
                return 0
            elif hypo == "A":
                return 0.5
            else:
                return 1

    # And here's how we use it.

    pmf = Monty("ABC")
    pmf.Update("B")
    pmf.Print()

    # ## The Suite class
    #
    # Most Bayesian updates look pretty much the same, especially the `Update` method.  So we can encapsulate the framework in a class, `Suite`, and create new classes that extend it.
    #

    # %psource Suite

    # Child classes of `Suite` inherit `Update` and provide `Likelihood`.
    #
    # So here's the short version of `Monty`

    class Monty(Suite):
        def Likelihood(self, data, hypo):
            if hypo == data:
                return 0
            elif hypo == "A":
                return 0.5
            else:
                return 1

    # And it works.

    pmf = Monty("ABC")
    pmf.Update("B")
    pmf.Print()

    # ## The M&M problem
    #
    # M&Ms are small candy-coated chocolates that come in a variety of
    # colors.  Mars, Inc., which makes M&Ms, changes the mixture of
    # colors from time to time.
    #
    # In 1995, they introduced blue M&Ms.  Before then, the color mix in
    # a bag of plain M&Ms was 30% Brown, 20% Yellow, 20% Red, 10%
    # Green, 10% Orange, 10% Tan.  Afterward it was 24% Blue , 20%
    # Green, 16% Orange, 14% Yellow, 13% Red, 13% Brown.
    #
    # Suppose a friend of mine has two bags of M&Ms, and he tells me
    # that one is from 1994 and one from 1996.  He won't tell me which is
    # which, but he gives me one M&M from each bag.  One is yellow and
    # one is green.  What is the probability that the yellow one came
    # from the 1994 bag?
    #
    # Here's a solution:

    class M_and_M(Suite):
        """Map from hypothesis (A or B) to probability."""

        mix94 = dict(brown=30, yellow=20, red=20, green=10, orange=10, tan=10, blue=0)

        mix96 = dict(blue=24, green=20, orange=16, yellow=14, red=13, brown=13, tan=0)

        hypoA = dict(bag1=mix94, bag2=mix96)
        hypoB = dict(bag1=mix96, bag2=mix94)

        hypotheses = dict(A=hypoA, B=hypoB)

        def Likelihood(self, data, hypo):
            """Computes the likelihood of the data under the hypothesis.

            hypo: string hypothesis (A or B)
            data: tuple of string bag, string color
            """
            bag, color = data
            mix = self.hypotheses[hypo][bag]
            like = mix[color]
            return like

    # And here's an update:

    suite = M_and_M("AB")
    suite.Update(("bag1", "yellow"))
    suite.Update(("bag2", "green"))
    suite.Print()

    # **Exercise:**  Suppose you draw another M&M from `bag1` and it's blue.  What can you conclude?  Run the update to confirm your intuition.

    suite.Update(("bag1", "blue"))
    suite.Print()

    # **Exercise:**  Now suppose you draw an M&M from `bag2` and it's blue.  What does that mean?  Run the update to see what happens.

    # Solution

    # throws ValueError: Normalize: total probability is zero.

    # suite.Update(('bag2', 'blue'))
