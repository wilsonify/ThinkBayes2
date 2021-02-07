"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT

**Exercise:** The GPS problem.  According to [Wikipedia]()
> GPS included a (currently disabled) feature called Selective Availability (SA) that adds intentional,
time varying errors of up to 100 meters (328 ft) to the publicly available navigation signals.
This was intended to deny an enemy the use of civilian GPS receivers for precision weapon guidance.
> Before it was turned off on May 2, 2000, typical SA errors were
about 50 m (164 ft) horizontally and about 100 m (328 ft) vertically.[10]
Because SA affects every GPS receiver in a given area almost equally,
a fixed station with an accurately known position can measure the SA error values and
transmit them to the local GPS receivers so they may correct their position fixes.
This is called Differential GPS or DGPS. DGPS also corrects for several other important sources of GPS errors,
particularly ionospheric delay, so it continues to be widely used even though SA has been turned off.
The ineffectiveness of SA in the face of widely available DGPS was a common argument for turning off SA,
and this was finally done by order of President Clinton in 2000.
Suppose it is 1 May 2000, and you are standing in a field that is 200m square.
You are holding a GPS unit that indicates that your location
is 51m north and 15m west of a known reference point in the middle of the field.
However, you know that each of these coordinates has been perturbed by a "feature"
that adds random errors with mean 0 and standard deviation 31) After taking one measurement, what should you believe about your position?
Note: Since the intentional errors are independent, you could solve this problem independently for X and Y.
But we'll treat it as a two-dimensional problem, partly for practice
and partly to see how we could extend the solution to handle dependent errors.
You can start with the code in gps.py.
2) Suppose that after one second the GPS updates your position and reports coordinates (48, 90).
What should you believe now?
3) Suppose you take 8 more measurements and get:
    (11.903060613102866, 19.79168669735705)
    (77.10743601503178, 39.87062906535289)
    (80.16596823095534, -12.797927542984425)
    (67.38157493119053, 83.52841028148538)
    (89.43965206875271, 20.52141889230797)
    (58.794021026248245, 30.23054016065644)
    (2.5844401241265302, 51.012041625783766)
    (45.58108994142448, 3.5718287379754585)
At this point, how certain are you about your location?

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
