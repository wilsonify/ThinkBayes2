import ppbm
from ppbm import (
    Chapter0_Prologue,
    Chapter1_Introduction,
    Chapter2_MorePyMC,
    Chapter3_MCMC,
    Chapter4_TheGreatestTheoremNeverTold,
    Chapter5_LossFunctions,
    Chapter6_Priorities,
    Chapter7_BayesianMachineLearning
)


def test_smoke():
    print("is anything on fire?")
    print(dir(ppbm))
    print(dir(Chapter0_Prologue))
    print(dir(Chapter1_Introduction))
    print(dir(Chapter2_MorePyMC))
    print(dir(Chapter3_MCMC))
    print(dir(Chapter4_TheGreatestTheoremNeverTold))
    print(dir(Chapter5_LossFunctions))
    print(dir(Chapter6_Priorities))
    print(dir(Chapter7_BayesianMachineLearning))
