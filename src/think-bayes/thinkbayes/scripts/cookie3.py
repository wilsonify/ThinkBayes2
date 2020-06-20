"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import thinkbayes


class Cookie(thinkbayes.Suite):
    """Suite to represent bowls of cookies."""

    def Likelihood(self, data, hypo):
        """The likelihood of the data under the hypothesis.

        data: string cookie type
        hypo: Hist of cookies
        """
        # compute the likelihood with the current bowls
        like = hypo[data] / hypo.Total()

        # update the bowl
        if like:
            hypo[data] -= 1

        return like


def main():
    # use Hists to represent the contents of the bowls
    bowl1 = thinkbayes.Hist(dict(vanilla=30, chocolate=10))
    bowl2 = thinkbayes.Hist(dict(vanilla=20, chocolate=20))

    # instantiate the suite
    suite = Cookie([bowl1, bowl2])

    print("After 1 vanilla")
    suite.Update("vanilla")
    for hypo, prob in suite.Items():
        print(hypo, prob)

    print("\nAfter 1 vanilla, 1 chocolate")
    suite.Update("chocolate")
    for hypo, prob in suite.Items():
        print(hypo, prob)

    print("\nAfter 1 vanilla, 1 chocolate, 1 vanilla")
    suite.Update("vanilla")
    for hypo, prob in suite.Items():
        print(hypo, prob)


if __name__ == "__main__":
    main()
