"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import numpy

import thinkbayes
from thinkbayes import thinkplot


class Electorate(thinkbayes.Suite):
    """Represents hypotheses about the state of the electorate."""

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: 
        data: 
        """
        like = 1
        return like


def main():
    hypos = numpy.linspace(0, 100, 101)
    suite = Electorate(hypos)

    thinkplot.plot_pdf_line(suite, label="prior")

    data = 1.1, 3.7, 53
    suite.update(data)

    thinkplot.plot_pdf_line(suite, label="posterior")
    thinkplot.show_plot()


if __name__ == "__main__":
    main()
