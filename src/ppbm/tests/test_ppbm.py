import ppbm
from ppbm import (
    Chapter0_Prologue
)

from ppbm.Chapter1_Introduction import Ch1_Introduction_PyMC3
from ppbm.Chapter2_MorePyMC import Ch2_MorePyMC_PyMC3
from ppbm.Chapter3_MCMC import Ch3_IntroMCMC_PyMC3
from ppbm.Chapter4_TheGreatestTheoremNeverTold import Ch4_LawOfLargeNumbers_PyMC3
from ppbm.Chapter5_LossFunctions import Ch5_LossFunctions_PyMC3
from ppbm.Chapter6_Priorities import Ch6_Priors_PyMC3


def test_smoke():
    print("is anything on fire?")
    print(dir(ppbm))
    print(dir(Chapter0_Prologue))
    print(dir(Ch1_Introduction_PyMC3))
    print(dir(Ch2_MorePyMC_PyMC3))
    print(dir(Ch3_IntroMCMC_PyMC3))
    print(dir(Ch4_LawOfLargeNumbers_PyMC3))
    print(dir(Ch5_LossFunctions_PyMC3))
    print(dir(Ch6_Priors_PyMC3))
