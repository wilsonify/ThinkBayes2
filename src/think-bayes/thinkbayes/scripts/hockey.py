"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import math

import thinkbayes
from thinkbayes import thinkplot
from thinkbayes.scripts import columns

USE_SUMMARY_DATA = True


class Hockey(thinkbayes.Suite):
    """Represents hypotheses about the scoring rate for a team."""

    def __init__(self, label=None):
        """Initializes the Hockey object.

        label: string
        """
        if USE_SUMMARY_DATA:
            # prior based on each team's average goals scored
            mu = 2.8
            sigma = 0.3
        else:
            # prior based on each pair-wise match-up
            mu = 2.8
            sigma = 0.85

        pmf = thinkbayes.MakeNormalPmf(mu, sigma, 4)
        thinkbayes.Suite.__init__(self, pmf, label=label)

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        Evaluates the Poisson PMF for lambda and k.

        hypo: goal scoring rate in goals per game
        data: goals scored in one game
        """
        lam = hypo
        k = data
        like = thinkbayes.EvalPoissonPmf(k, lam)
        return like


def make_goal_pmf(suite, high=10):
    """Makes the distribution of goals scored, given distribution of lam.

    suite: distribution of goal-scoring rate
    high: upper bound

    returns: Pmf of goals per game
    """
    metapmf = thinkbayes.Pmf()

    for lam, prob in suite.Items():
        pmf = thinkbayes.MakePoissonPmf(lam, high)
        metapmf.Set(pmf, prob)

    mix = thinkbayes.MakeMixture(metapmf, label=suite.label)
    return mix


def make_goal_time_pmf(suite):
    """Makes the distribution of time til first goal.

    suite: distribution of goal-scoring rate

    returns: Pmf of goals per game
    """
    metapmf = thinkbayes.Pmf()

    for lam, prob in suite.Items():
        pmf = thinkbayes.MakeExponentialPmf(lam, high=2, n=2001)
        metapmf.Set(pmf, prob)

    mix = thinkbayes.MakeMixture(metapmf, label=suite.label)
    return mix


class Game(object):
    """Represents a game.

    Attributes are set in columns.read_csv.
    """

    convert = dict()

    def clean(self):
        # noinspection PyUnresolvedReferences
        self.goals = self.pd1 + self.pd2 + self.pd3


def read_hockey_data(filename="hockey_data.csv"):
    """Read game scores from the data file.

    filename: string
    """
    game_list = columns.read_csv(filename, Game)

    # map from gameID to list of two games
    games = {}
    for game in game_list:
        if game.season != 2011:
            continue
        key = game.game
        games.setdefault(key, []).append(game)

    # map from (team1, team2) to (score1, score2)
    pairs = {}
    for key, pair in games.items():
        t1, t2 = pair
        key = t1.team, t2.team
        entry = t1.total, t2.total
        pairs.setdefault(key, []).append(entry)

    process_scores_teamwise(pairs)
    process_scores_pairwise(pairs)


def process_scores_pairwise(pairs):
    """Average number of goals for each team against each opponent.

    pairs: map from (team1, team2) to (score1, score2)
    """
    # map from (team1, team2) to list of goals scored
    goals_scored = {}
    for key, entries in pairs.items():
        t1, t2 = key
        for entry in entries:
            g1, g2 = entry
            goals_scored.setdefault((t1, t2), []).append(g1)
            goals_scored.setdefault((t2, t1), []).append(g2)

    # make a list of average goals scored
    lams = []
    for key, goals in goals_scored.items():
        if len(goals) < 3:
            continue
        lam = thinkbayes.Mean(goals)
        lams.append(lam)

    # make the distribution of average goals scored
    cdf = thinkbayes.MakeCdfFromList(lams)
    thinkplot.plot_cdf_line(cdf)
    thinkplot.show_plot()

    mu, var = thinkbayes.MeanVar(lams)
    print("mu, sig", mu, math.sqrt(var))

    print("BOS v VAN", pairs["BOS", "VAN"])


def process_scores_teamwise(pairs):
    """Average number of goals for each team.

    pairs: map from (team1, team2) to (score1, score2)
    """
    # map from team to list of goals scored
    goals_scored = {}
    for key, entries in pairs.items():
        t1, t2 = key
        for entry in entries:
            g1, g2 = entry
            goals_scored.setdefault(t1, []).append(g1)
            goals_scored.setdefault(t2, []).append(g2)

    # make a list of average goals scored
    lams = []
    for key, goals in goals_scored.items():
        lam = thinkbayes.Mean(goals)
        lams.append(lam)

    # make the distribution of average goals scored
    cdf = thinkbayes.MakeCdfFromList(lams)
    thinkplot.plot_cdf_line(cdf)
    thinkplot.show_plot()

    mu, var = thinkbayes.MeanVar(lams)
    print("mu, sig", mu, math.sqrt(var))


def main():
    # ReadHockeyData()
    # return

    formats = ["pdf", "eps"]

    suite1 = Hockey("bruins")
    suite2 = Hockey("canucks")

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)
    thinkplot.plot_pmf_line(suite1)
    thinkplot.plot_pmf_line(suite2)
    thinkplot.save_plot(
        root="hockey0", xlabel="Goals per game", ylabel="Probability", formats=formats
    )

    suite1.UpdateSet([0, 2, 8, 4])
    suite2.UpdateSet([1, 3, 1, 0])

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)
    thinkplot.plot_pmf_line(suite1)
    thinkplot.plot_pmf_line(suite2)
    thinkplot.save_plot(
        root="hockey1", xlabel="Goals per game", ylabel="Probability", formats=formats
    )

    goal_dist1 = make_goal_pmf(suite1)
    goal_dist2 = make_goal_pmf(suite2)

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)
    thinkplot.plot_pmf_line(goal_dist1)
    thinkplot.plot_pmf_line(goal_dist2)
    thinkplot.save_plot(
        root="hockey2", xlabel="Goals", ylabel="Probability", formats=formats
    )

    time_dist1 = make_goal_time_pmf(suite1)
    time_dist2 = make_goal_time_pmf(suite2)

    print("MLE bruins", suite1.MaximumLikelihood())
    print("MLE canucks", suite2.MaximumLikelihood())

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)
    thinkplot.plot_pmf_line(time_dist1)
    thinkplot.plot_pmf_line(time_dist2)
    thinkplot.save_plot(
        root="hockey3", xlabel="Games until goal", ylabel="Probability", formats=formats
    )

    diff = goal_dist1 - goal_dist2
    p_win = diff.ProbGreater(0)
    p_loss = diff.ProbLess(0)
    p_tie = diff.Prob(0)

    print(p_win, p_loss, p_tie)

    p_overtime = thinkbayes.PmfProbLess(time_dist1, time_dist2)
    p_adjust = thinkbayes.PmfProbEqual(time_dist1, time_dist2)
    p_overtime += p_adjust / 2
    print("p_overtime", p_overtime)

    print(p_overtime * p_tie)
    p_win += p_overtime * p_tie
    print("p_win", p_win)

    # win the next two
    p_series = p_win ** 2

    # split the next two, win the third
    p_series += 2 * p_win * (1 - p_win) * p_win

    print("p_series", p_series)


if __name__ == "__main__":
    main()
