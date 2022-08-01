"""
This is based on a notebook of example code from Think Bayes.
"""
from thinkbayes import Pmf
from thinkbayes.c02_bayes_theorem import Cookie, FullMonty, Monty
from thinkbayes.scripts.mandm import MAndM



def test_pmf(six_sided_die_pmf):
    # A faster way to make a Pmf is to provide a sequence of values.
    # The constructor adds the values to the Pmf and then normalizes:
    pmf = Pmf([1, 2, 3, 4, 5, 6])
    assert pmf == six_sided_die_pmf


def test_Prob(six_sided_die_pmf):
    six_sided_die_pmf.Prob(1)  # To extract a value from a Pmf, you can use `Prob`
    print(six_sided_die_pmf[1])  # Or you can use the bracket operator.
    assert six_sided_die_pmf.Prob(1) == six_sided_die_pmf[1]


def test_exclusive(six_sided_die_pmf):
    assert six_sided_die_pmf[7] == 0  # not in the Pmf, the result is 0.


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


def cookie_update_strategy(self, body: dict):
    # We can confirm that we get the same result.

    pmf = Cookie(["Bowl1", "Bowl2"])
    pmf.Update("vanilla")
    pmf.Print()

    # But this implementation is more general; it can handle any sequence of data.

    # +
    dataset = ["vanilla", "chocolate", "vanilla"]
    for data in dataset:
        pmf.Update(data)

    pmf.Print()


def monty_hall_strategy(self, body: dict):
    pmf = FullMonty("ABC")
    pmf.Update("B")
    pmf.Print()

    pmf = Monty("ABC")
    pmf.Update("B")
    pmf.Print()


def m_and_m_strategy(self, body: dict):
    suite = MAndM("AB")
    suite.Update(("bag1", "yellow"))
    suite.Update(("bag2", "green"))
    suite.Print()

    # **Exercise:**
    # Suppose you draw another M&M from `bag1` and it's blue.
    # What can you conclude?  Run the update to confirm your intuition.

    suite.Update(("bag1", "blue"))
    suite.Print()

    # **Exercise:**
    # Now suppose you draw an M&M from `bag2` and it's blue.
    # What does that mean?  Run the update to see what happens.

    # +
    # Solution goes here
