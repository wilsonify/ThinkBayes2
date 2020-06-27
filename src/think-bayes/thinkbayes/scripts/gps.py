"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from itertools import product

import numpy
import thinkbayes
from thinkbayes import thinkplot


class Gps(thinkbayes.Suite, thinkbayes.Joint):
    """Represents hypotheses about your location in the field."""

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo:
        data:
        """
        std = 30
        meanx, meany = hypo
        x, y = data
        like = thinkbayes.eval_normal_pdf(x, meanx, std)
        like *= thinkbayes.eval_normal_pdf(y, meany, std)
        return like


def main():
    coords = numpy.linspace(-100, 100, 101)
    joint = Gps(product(coords, coords))

    joint.update((51, -15))
    joint.update((48, 90))

    pairs = [
        (11.903060613102866, 19.79168669735705),
        (77.10743601503178, 39.87062906535289),
        (80.16596823095534, -12.797927542984425),
        (67.38157493119053, 83.52841028148538),
        (89.43965206875271, 20.52141889230797),
        (58.794021026248245, 30.23054016065644),
        (2.5844401241265302, 51.012041625783766),
        (45.58108994142448, 3.5718287379754585),
    ]

    joint.update_set(pairs)

    thinkplot.pre_plot(2)
    pdfx = joint.marginal(0)
    pdfy = joint.marginal(1)
    thinkplot.plot_pdf_line(pdfx, label="posterior x")
    thinkplot.plot_pdf_line(pdfy, label="posterior y")
    thinkplot.show_plot()

    print(pdfx.mean(), pdfx.std())
    print(pdfy.mean(), pdfy.std())


if __name__ == "__main__":
    main()
