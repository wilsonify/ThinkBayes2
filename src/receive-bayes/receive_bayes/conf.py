import os

import pandas as pd

import thinkbayes

CONFTESTDIR = os.path.abspath(os.path.dirname(__file__))
TESTDIR = os.path.abspath(os.path.join(CONFTESTDIR, os.pardir))
DATADIR = os.path.join(TESTDIR, "data")

gss = pd.read_csv(f'{DATADIR}/gss_bayes.csv', index_col=0)

six_sided_die_pmf = thinkbayes.Pmf()
for x in [1, 2, 3, 4, 5, 6]:
    six_sided_die_pmf[x] = 1
six_sided_die_pmf.Normalize()

drp_scores_df = pd.read_csv(f"{DATADIR}/drp_scores.csv", skiprows=21, delimiter="\t")

flea_beetles_df = pd.read_csv(f"{DATADIR}/flea_beetles.csv", delimiter="\t")
