"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf

pmf = Pmf()
pmf.set("Bowl1", 0.5)
pmf.set("Bowl2", 0.5)

pmf.mult("Bowl1", 0.75)
pmf.mult("Bowl2", 0.5)

pmf.normalize()

print(pmf.prob("Bowl1"))
