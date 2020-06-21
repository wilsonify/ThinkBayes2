"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import thinkbayes
from thinkbayes import thinkplot


class Electorate(thinkbayes.Suite):
    """Represents hypotheses about the state of the electorate."""

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: fraction of the population that supports your candidate
        data: poll results
        """
        bias, std, result = data
        error = result - hypo
        like = thinkbayes.EvalNormalPdf(error, bias, std)
        return like


def main():
    hypos = range(0, 101)
    suite = Electorate(hypos)

    thinkplot.pre_plot(3)
    thinkplot.plot_pdf_line(suite, label="prior")

    data = 1.1, 3.7, 53
    suite.update(data)
    thinkplot.plot_pdf_line(suite, label="posterior1")
    thinkplot.save_plot(
        root="electorate1",
        xlabel="percentage of electorate",
        ylabel="PMF",
        formats=["png"],
        clf=False,
    )

    print(suite.Mean())
    print(suite.Std())
    print(suite.ProbLess(50))

    data = -2.3, 4.1, 49
    suite.update(data)

    thinkplot.plot_pdf_line(suite, label="posterior2")
    thinkplot.save_plot(
        root="electorate2",
        xlabel="percentage of electorate",
        ylabel="PMF",
        formats=["png"],
    )

    print(suite.Mean())
    print(suite.Std())
    print(suite.ProbLess(50))


if __name__ == "__main__":
    main()
