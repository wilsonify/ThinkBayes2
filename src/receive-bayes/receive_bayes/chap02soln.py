"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf
from thinkbayes.c02_bayes_theorem import (
    Cookie2, Monty2, Monty3, M_and_M2
)

def cookie_strategy(self, body: dict):
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

    # We can confirm that we get the same result.

    pmf = Cookie2(["Bowl1", "Bowl2"])
    pmf.Update("vanilla")
    pmf.Print()

    # But this implementation is more general; it can handle any sequence of data.

    dataset = ["vanilla", "chocolate", "vanilla"]
    for data in dataset:
        pmf.Update(data)

    pmf.Print()


def mhp_strategy(self, body: dict):
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

    # And here's how we use it.

    pmf = Monty2("ABC")
    pmf.Update("B")
    pmf.Print()

    # ## The Suite class
    #
    # Most Bayesian updates look pretty much the same, especially the `Update` method.
    # So we can encapsulate the framework in a class, `Suite`, and create new classes that extend it.
    #

    # %psource Suite

    # Child classes of `Suite` inherit `Update` and provide `Likelihood`.
    #
    # So here's the short version of `Monty`

    # And it works.

    pmf = Monty3("ABC")
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

    # And here's an update:

    suite = M_and_M2("AB")
    suite.Update(("bag1", "yellow"))
    suite.Update(("bag2", "green"))
    suite.Print()

    # **Exercise:**
    # Suppose you draw another M&M from `bag1` and it's blue.
    # What can you conclude?
    # Run the update to confirm your intuition.

    suite.Update(("bag1", "blue"))
    suite.Print()

    # **Exercise:**
    # Now suppose you draw an M&M from `bag2` and it's blue.
    # What does that mean?
    # Run the update to see what happens.

    # Solution

    # throws ValueError: Normalize: total probability is zero.

    # suite.Update(('bag2', 'blue'))
