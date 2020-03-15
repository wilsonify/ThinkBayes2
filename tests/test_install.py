"""
This file contains code used in "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2018 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from __future__ import print_function, division

import logging
import math
import numpy

from matplotlib import pyplot

import thinkbayes
from thinkbayes import thinkplot


def test_smoke():
    logging.warning("is anything on fire")


def test_RenderPdf():
    xs, ys = thinkplot.RenderPdf(100, 15)

    n = 34
    pyplot.fill_between(xs[-n:], ys[-n:], y2=0.0001, color='blue', alpha=0.2)
    s = 'Congratulations!\nIf you got this far,\nyou must be here.'
    d = dict(shrink=0.05)
    pyplot.annotate(s, [127, 0.002], xytext=[80, 0.005], arrowprops=d)

    thinkplot.Plot(xs, ys)
    thinkplot.Show(
        title='Distribution of Persistence',
        xlabel='Persistence quotient',
        ylabel='PDF',
        legend=False
    )
