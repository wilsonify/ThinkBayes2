"""
Bayesian Statistics Made Simple
Code and exercises from my workshop on Bayesian statistics in Python.
Copyright 2018 Allen Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy as np

from thinkbayes import Suite
from thinkbayes import thinkplot

import warnings


def test_bandit():
    # ## The likelihood function
    #
    #
    # Here's a definition for `Bandit`, which extends `Suite` and defines a likelihood function that computes the probability of the data (win or lose) for a given value of `x` (the probability of win).
    #
    # Note that `hypo` is in the range 0 to 100.

    class Bandit(Suite):
        def Likelihood(self, data, hypo):
            """
            hypo is the prob of win (0-100)
            data is a string, either 'W' or 'L'
            """
            x = hypo / 100
            if data == "W":
                return x
            else:
                return 1 - x

    # We'll start with a uniform distribution from 0 to 100.

    bandit = Bandit(range(101))
    thinkplot.Pdf(bandit)
    thinkplot.Config(xlabel="x", ylabel="Probability")

    # Now we can update with a single loss:

    bandit.Update("L")
    thinkplot.Pdf(bandit)
    thinkplot.Config(xlabel="x", ylabel="Probability", legend=False)

    # Another loss:

    bandit.Update("L")
    thinkplot.Pdf(bandit)
    thinkplot.Config(xlabel="x", ylabel="Probability", legend=False)

    # And a win:

    bandit.Update("W")
    thinkplot.Pdf(bandit)
    thinkplot.Config(xlabel="x", ylabel="Probability", legend=False)

    # Starting over, here's what it looks like after 1 win and 9 losses.

    bandit = Bandit(range(101))

    for outcome in "WLLLLLLLLL":
        bandit.Update(outcome)

    thinkplot.Pdf(bandit)
    thinkplot.Config(xlabel="x", ylabel="Probability", legend=False)

    # The posterior mean is about 17%

    bandit.Mean()

    # The most likely value is the observed proportion 1/10

    bandit.MAP()

    # The posterior credible interval has a 90% chance of containing the true value (provided that the prior distribution truly represents our background knowledge).

    bandit.CredibleInterval(90)

    # ## Multiple bandits

    # Now suppose we have several bandits and we want to decide which one to play.

    # For this example, we have 4 machines with these probabilities:

    actual_probs = [0.10, 0.20, 0.30, 0.40]

    # The following function simulates playing one machine once.

    from random import random
    from collections import Counter

    counter = Counter()

    def flip(p):
        return random() < p

    def play(i):
        counter[i] += 1
        p = actual_probs[i]
        if flip(p):
            return "W"
        else:
            return "L"

    # Here's a test, playing machine 3 twenty times:

    for i in range(20):
        result = play(3)
        print(result, end=" ")

    # Now I'll make 4 `Bandit` objects to represent our beliefs about the 4 machines.

    prior = range(101)
    beliefs = [Bandit(prior) for i in range(4)]

    # This function displays the four posterior distributions

    options = dict(yticklabels="invisible")

    def plot(beliefs, **options):
        thinkplot.preplot(rows=2, cols=2)
        for i, b in enumerate(beliefs):
            thinkplot.subplot(i + 1)
            thinkplot.Pdf(b, label=i)
            thinkplot.Config(**options)

    plot(beliefs, legend=True)

    # Now suppose we play each machine 10 times.  This function updates our beliefs about one of the machines based on one outcome.

    def update(beliefs, i, outcome):
        beliefs[i].Update(outcome)

    for i in range(4):
        for _ in range(10):
            outcome = play(i)
            update(beliefs, i, outcome)

    plot(beliefs, legend=True)

    # After playing each machine 10 times, we have some information about their probabilies:

    [belief.Mean() for belief in beliefs]

    # ## Bayesian Bandits
    #
    # To get more information, we could play each machine 100 times, but while we are gathering data, we are not making good use of it.  The kernel of the Bayesian Bandits algorithm is that is collects and uses data at the same time.  In other words, it balances exploration and exploitation.
    #
    # The following function chooses among the machines so that the probability of choosing each machine is proportional to its "probability of superiority".
    #
    # `Random` chooses a value from the posterior distribution.
    #
    # `argmax` returns the index of the machine that chose the highest value.

    def choose(beliefs):
        ps = [b.Random() for b in beliefs]
        return np.argmax(ps)

    # Here's an example.

    choose(beliefs)

    # Putting it all together, the following function chooses a machine, plays once, and updates `beliefs`:

    def choose_play_update(beliefs, verbose=False):
        i = choose(beliefs)
        outcome = play(i)
        update(beliefs, i, outcome)
        if verbose:
            print(i, outcome, beliefs[i].Mean())

    # Here's an example

    counter = Counter()
    choose_play_update(beliefs, verbose=True)

    # ## Trying it out

    # Let's start again with a fresh set of machines:

    beliefs = [Bandit(prior) for i in range(4)]

    # Now we can play a few times and see how `beliefs` gets updated:

    num_plays = 100

    for i in range(num_plays):
        choose_play_update(beliefs)

    plot(beliefs)

    # We can summarize `beliefs` by printing the posterior mean and credible interval:

    for i, b in enumerate(beliefs):
        print(b.Mean(), b.CredibleInterval(90))

    # The credible intervals usually contain the true values (10, 20, 30, and 40).
    #
    # The estimates are still rough, especially for the lower-probability machines.  But that's a feature, not a bug: the goal is to play the high-probability machines most often.  Making the estimates more precise is a means to that end, but not an end itself.
    #
    # Let's see how many times each machine got played.  If things go according to play, the machines with higher probabilities should get played more often.

    for machine, count in sorted(counter.items()):
        print(machine, count)

    # **Exercise:**  Go back and run this section again with a different value of `num_play` and see how it does.
