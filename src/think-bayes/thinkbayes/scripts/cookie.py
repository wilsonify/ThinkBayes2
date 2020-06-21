"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from thinkbayes import Pmf

pmf = Pmf()
pmf.Set("Bowl1", 0.5)
pmf.Set("Bowl2", 0.5)

pmf.Mult("Bowl1", 0.75)
pmf.Mult("Bowl2", 0.5)

pmf.Normalize()

print(pmf.Prob("Bowl1"))
