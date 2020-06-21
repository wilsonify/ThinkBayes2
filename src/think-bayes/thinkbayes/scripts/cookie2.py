"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf


class Cookie(Pmf):
    """A map from string bowl ID to probablity."""

    def __init__(self, hypos):
        """Initialize self.

        hypos: sequence of string bowl IDs
        """
        Pmf.__init__(self)
        for hypo in hypos:
            self.set(hypo, 1)
        self.normalize()

    def update(self, data):
        """Updates the PMF with new data.

        data: string cookie type
        """
        for hypo in self.values():
            like = self.likelihood(data, hypo)
            self.mult(hypo, like)
        self.normalize()

    mixes = {
        "Bowl1": dict(vanilla=0.75, chocolate=0.25),
        "Bowl2": dict(vanilla=0.5, chocolate=0.5),
    }

    def likelihood(self, data, hypo):
        """The likelihood of the data under the hypothesis.

        data: string cookie type
        hypo: string bowl ID
        """
        mix = self.mixes[hypo]
        like = mix[data]
        return like


def main():
    hypos = ["Bowl1", "Bowl2"]

    pmf = Cookie(hypos)

    pmf.update("vanilla")

    for hypo, prob in pmf.items():
        print(hypo, prob)


if __name__ == "__main__":
    main()
