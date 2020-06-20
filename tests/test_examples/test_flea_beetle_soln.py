"""
Think Bayes
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import pandas as pd
from thinkbayes import Cdf, Suite
from thinkbayes import thinkplot


# ### The flea beetle problem
#
# Different species of flea beetle can be distinguished by the width and angle of the aedeagus.  The data below includes measurements and know species classification for 74 specimens.
#
# Suppose you discover a new specimen under conditions where it is equally likely to be any of the three species.  You measure the aedeagus and width 140 microns and angle 15 (in multiples of 7.5 degrees).  What is the probability that it belongs to each species?
#
#
# This problem is based on [this data story on DASL](https://web.archive.org/web/20160304083805/http://lib.stat.cmu.edu/DASL/Datafiles/FleaBeetles.html)
#
# Datafile Name: Flea Beetles
#
# Datafile Subjects: Biology
#
# Story Names: Flea Beetles
#
# Reference: Lubischew, A.A. (1962) On the use of discriminant functions in taxonomy. Biometrics, 18, 455-477. Also found in: Hand, D.J., et al. (1994) A Handbook of Small Data Sets, London: Chapman & Hall, 254-255.
#
# Authorization: Contact Authors
#
# Description: Data were collected on the genus of flea beetle Chaetocnema, which contains three species: concinna (Con), heikertingeri (Hei), and heptapotamica (Hep). Measurements were made on the width and angle of the aedeagus of each beetle. The goal of the original study was to form a classification rule to distinguish the three species.
#
# Number of cases: 74
#
# Variable Names:
#
# Width: The maximal width of aedeagus in the forpart (in microns)
#
# Angle: The front angle of the aedeagus (1 unit = 7.5 degrees)
#
# Species: Species of flea beetle from the genus Chaetocnema
#
# We can read the data from this file:


def test_flea_beetles():
    df = pd.read_csv("../data/flea_beetles.csv", delimiter="\t")
    df.head()

    # Here's what the distributions of width look like.

    def plot_cdfs(df, col):
        for name, group in df.groupby("Species"):
            cdf = Cdf(group[col], label=name)
            thinkplot.Cdf(cdf)

        thinkplot.decorate(xlabel=col, ylabel="CDF", loc="lower right")

    plot_cdfs(df, "Width")

    # And the distributions of angle.

    plot_cdfs(df, "Angle")

    # I'll group the data by species and compute summary statistics.

    grouped = df.groupby("Species")

    # Here are the means.

    means = grouped.mean()

    # And the standard deviations.

    stddevs = grouped.std()

    # And the correlations.

    for name, group in grouped:
        corr = group.Width.corr(group.Angle)
        print(name, corr)

    # Those correlations are small enough that we can get an acceptable approximation by ignoring them, but we might want to come back later and write a complete solution that takes them into account.
    #
    # ### The likelihood function
    #
    # To support the likelihood function, I'll make a dictionary for each attribute that contains a `norm` object for each species.

    from scipy.stats import norm

    dist_width = {}
    dist_angle = {}
    for name, group in grouped:
        dist_width[name] = norm(group.Width.mean(), group.Width.std())
        dist_angle[name] = norm(group.Angle.mean(), group.Angle.std())

    # Now we can write the likelihood function concisely.

    class Beetle(Suite):
        def Likelihood(self, data, hypo):
            """
            data: sequence of width, height
            hypo: name of species
            """
            width, angle = data
            name = hypo

            like = dist_width[name].pdf(width)
            like *= dist_angle[name].pdf(angle)
            return like

    # The hypotheses are the species names:

    hypos = grouped.groups.keys()

    # We'll start with equal priors

    suite = Beetle(hypos)
    suite.Print()

    # Now we can update with the data and print the posterior.

    suite.Update((140, 15))
    suite.Print()

    # Based on these measurements, the specimen is very likely to be an example of *Chaetocnema concinna*.
