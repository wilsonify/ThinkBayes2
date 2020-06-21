"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Suite


class Monty(Suite):
    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

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
    suite = Monty("ABC")
    suite.update("B")
    suite.print()


if __name__ == "__main__":
    main()
