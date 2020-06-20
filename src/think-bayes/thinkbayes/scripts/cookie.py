"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""


from src.thinkbayes2 import Pmf

pmf = Pmf()
pmf.Set("Bowl 1", 0.5)
pmf.Set("Bowl 2", 0.5)

pmf.Mult("Bowl 1", 0.75)
pmf.Mult("Bowl 2", 0.5)

pmf.Normalize()

print(pmf.Prob("Bowl 1"))
