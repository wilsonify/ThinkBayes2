"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Suite


class Dice(Suite):
    """Represents hypotheses about which die was rolled."""

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: integer number of sides on the die
        data: integer die roll
        """
        if hypo < data:
            return 0
        else:
            return 1.0 / hypo


def main():
    suite = Dice([4, 6, 8, 12, 20])

    suite.update(6)
    print("After one 6")
    suite.print()

    for roll in [4, 8, 7, 7, 2]:
        suite.update(roll)

    print("After more rolls")
    suite.print()


if __name__ == "__main__":
    main()
