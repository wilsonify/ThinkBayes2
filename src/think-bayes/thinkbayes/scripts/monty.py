"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf


class Monty(Pmf):
    """Map from string location of car to probability"""

    def __init__(self, hypos):
        """Initialize the distribution.

        hypos: sequence of hypotheses
        """
        Pmf.__init__(self)
        for hypo in hypos:
            self.set(hypo, 1)
        self.normalize()

    def update(self, data):
        """Updates each hypothesis based on the data.

        data: any representation of the data
        """
        for hypo in self.values():
            like = self.likelihood(data, hypo)
            self.mult(hypo, like)
        self.normalize()

    @staticmethod
    def likelihood(data, hypo):
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


def main():
    hypos = "ABC"
    pmf = Monty(hypos)

    data = "B"
    pmf.update(data)

    for hypo, prob in sorted(pmf.items()):
        print(hypo, prob)


if __name__ == "__main__":
    main()
