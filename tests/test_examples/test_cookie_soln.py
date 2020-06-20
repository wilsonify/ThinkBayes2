"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
from thinkbayes import Hist, Pmf


def test_cookie_problem():
    # ## The cookie problem
    #
    # Here's the original statement of the cookie problem:
    #
    # > Suppose there are two bowls of cookies. Bowl 1 contains 30 vanilla cookies and 10 chocolate cookies. Bowl 2 contains 20 of each.
    #
    # > Now suppose you choose one of the bowls at random and, without looking, select a cookie at random. The cookie is vanilla. What is the probability that it came from Bowl 1?
    #
    # If we only draw one cookie, this problem is simple, but if we draw more than one cookie, there is a complication: do we replace the cookie after each draw, or not?
    #
    # If we replace the cookie, the proportion of vanilla and chocolate cookies stays the same, and we can perform multiple updates with the same likelihood function.
    #
    # If we *don't* replace the cookie, the proportions change and we have to keep track of the number of cookies in each bowl.
    #
    # **Exercise:**
    #
    # Modify the solution from the book to handle selection without replacement.
    #
    # Hint: Add instance variables to the `Cookie` class to represent the hypothetical state of the bowls, and modify the `Likelihood` function accordingly.
    #
    # To represent the state of a Bowl, you might want to use the `Hist` class from `thinkbayes2`.

    # Solution

    # We'll need an object to keep track of the number of cookies in each bowl.
    # I use a Hist object, defined in thinkbayes2:

    bowl1 = Hist(dict(vanilla=30, chocolate=10))
    bowl2 = Hist(dict(vanilla=20, chocolate=20))

    bowl1.Print()

    # Solution

    # Now I'll make a Pmf that contains the two bowls, giving them equal probability.

    pmf = Pmf([bowl1, bowl2])
    pmf.Print()

    # Solution

    # Here's a likelihood function that takes `hypo`, which is one of
    # the Hist objects that represents a bowl, and `data`, which is either
    # 'vanilla' or 'chocolate'.

    # `likelihood` computes the likelihood of the data under the hypothesis,
    # and as a side effect, it removes one of the cookies from `hypo`

    def likelihood(hypo, data):
        like = hypo[data] / hypo.Total()
        if like:
            hypo[data] -= 1
        return like

    # Solution

    # Now for the update.  We have to loop through the hypotheses and
    # compute the likelihood of the data under each hypothesis.

    def update(pmf, data):
        for hypo in pmf:
            pmf[hypo] *= likelihood(hypo, data)
        return pmf.Normalize()

    # Solution

    # Here's the first update.  The posterior probabilities are the
    # same as what we got before, but notice that the number of cookies
    # in each Hist has been updated.

    update(pmf, "vanilla")
    pmf.Print()

    # Solution

    # So when we update again with a chocolate cookies, we get different
    # likelihoods, and different posteriors.

    update(pmf, "chocolate")
    pmf.Print()

    # Solution

    # If we get 10 more chocolate cookies, that eliminates Bowl 1 completely

    for i in range(10):
        update(pmf, "chocolate")
        print(pmf[bowl1])
