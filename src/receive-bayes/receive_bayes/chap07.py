"""
This is based on code and exercises from Think Bayes: Chapter 7.
Think Bayes: Chapter 7
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np
from scipy.stats import expon

from thinkbayes import EvalExponentialCdf
from thinkbayes import EvalPoissonPmf
from thinkbayes import MakeExponentialPmf
from thinkbayes import MakeGammaPmf
from thinkbayes import MakePoissonPmf
from thinkbayes.c07_mixture import Hockey2, MakeGoalPmf2, MakeGoalTimePmf2


def single_game_goals_strategy(self, body: dict):
    # Suppose that goal scoring in hockey is well modeled by a Poisson process,
    # and that the long-run goal-scoring rate of the Boston Bruins against the Vancouver Canucks is 2.9 goals per game.
    # In their next game, what is the probability that the Bruins score exactly 3 goals?
    # Plot the PMF of `k`, the number of goals they score in a game.
    long_run_goal_scoring_rate = body['long_run_goal_scoring_rate_of_Boston_Bruins']
    single_game_goals = body['single_game_goals']
    result = EvalPoissonPmf(single_game_goals, long_run_goal_scoring_rate)
    self.publish(result)


def multiple_game_goals_strategy(self, body: dict):
    # Assuming again that the goal scoring rate is 2.9,
    # what is the probability of scoring a total of 9 goals in three games?
    # Answer this question two ways:
    # 1. Compute the distribution of goals scored in one game
    # and then add it to itself twice to find the distribution of goals scored in 3 games.
    # 2.  Use the Poisson PMF with parameter $\lambda t$,
    # where $\lambda$ is the rate in goals per game
    # and $t$ is the duration in games.
    long_run_goal_scoring_rate = body['long_run_goal_scoring_rate_of_Boston_Bruins']
    multiple_game_goals = body['multiple_game_goals']
    ngames = body['ngames']
    pmf = MakePoissonPmf(long_run_goal_scoring_rate, high=ngames * 10)
    total = pmf + pmf + pmf
    result = total.Prob(multiple_game_goals)
    self.publish(result)


def first_goal_strategy(self, body: dict):
    # **Exercise:** Suppose that the long-run goal-scoring rate of the
    # Canucks against the Bruins is 2.6 goals per game.  Plot the distribution
    # of `t`, the time until the Canucks score their first goal.
    # In their next game, what is the probability that the Canucks score
    # during the first period (that is, the first third of the game)?
    # Hint: `thinkbayes2` provides `MakeExponentialPmf` and `EvalExponentialCdf`.
    pmf = MakeExponentialPmf(lam=2.6, high=2.5)
    expon.cdf(1 / 3, scale=1 / 2.6)
    EvalExponentialCdf(1 / 3, 2.6)


def shut_out_strategy():
    # Assuming again that the goal scoring rate is 2.8,
    # what is the probability that the Canucks get shut out
    # (that is, don't score for an entire game)?
    # Answer this question two ways,
    # using the CDF of the exponential distribution and the PMF of the Poisson distribution.
    logging.info("%r", f"1 - EvalExponentialCdf(1, 2.6) = {1 - EvalExponentialCdf(1, 2.6)}")
    EvalPoissonPmf(0, 2.6)


def hockey_strategy():
    # ## The Boston Bruins problem
    # The `Hockey` suite contains hypotheses about the goal scoring rate for one team against the other.
    # The prior is Gaussian, with mean and variance based on previous games in the league.
    # The Likelihood function takes as data the number of goals scored in a game.
    # Now we can initialize a suite for each team:
    suite1 = Hockey2("bruins")
    suite2 = Hockey2("canucks")
    # Here's what the priors look like:
    # And we can update each suite with the scores from the first 4 games.
    suite1.UpdateSet([0, 2, 8, 4])
    suite2.UpdateSet([1, 3, 1, 0])
    logging.info("%r", f"suite1.Mean() = {suite1.Mean()}")
    logging.info("%r", f"suite2.Mean() = {suite2.Mean()}")

    # To predict the number of goals scored in the next game we can compute,
    # for each hypothetical value of $\lambda$,
    # a Poisson distribution of goals scored,
    # then make a weighted mixture of Poissons:
    # Here's what the results look like.
    goal_dist1 = MakeGoalPmf2(suite1)
    goal_dist2 = MakeGoalPmf2(suite2)

    logging.info("%r", f"goal_dist1.Mean() = {goal_dist1.Mean()}")
    logging.info("%r", f"goal_dist2.Mean() = {goal_dist2.Mean()}")

    # Now we can compute the probability that the Bruins win, lose, or tie in regulation time.
    diff = goal_dist1 - goal_dist2
    p_win = diff.ProbGreater(0)
    p_loss = diff.ProbLess(0)
    p_tie = diff.Prob(0)

    print(dict(
        win_p=p_win,
        tie_p=p_tie,
        loss_p=p_loss
    ))


def overtime_strategy():
    # If the game goes into overtime,
    # we have to compute the distribution of `t`,
    # the time until the first goal, for each team.
    # For each hypothetical value of $\lambda$, the distribution of `t` is exponential,
    # so the predictive distribution is a mixture of exponentials.

    # Here's what the predictive distributions for `t` look like.
    suite1 = Hockey2("bruins")
    suite2 = Hockey2("canucks")
    suite1.UpdateSet([0, 2, 8, 4])
    suite2.UpdateSet([1, 3, 1, 0])
    goal_dist1 = MakeGoalPmf2(suite1)
    goal_dist2 = MakeGoalPmf2(suite2)
    diff = goal_dist1 - goal_dist2
    p_win = diff.ProbGreater(0)
    p_loss = diff.ProbLess(0)
    p_tie = diff.Prob(0)
    time_dist1 = MakeGoalTimePmf2(suite1)
    time_dist2 = MakeGoalTimePmf2(suite2)
    logging.info("%r", f"time_dist1.Mean() = {time_dist1.Mean()}")
    logging.info("%r", f"time_dist2.Mean() = {time_dist2.Mean()}")

    # In overtime the first team to score wins,
    # so the probability of winning is the probability of generating a smaller value of `t`:

    p_win_in_overtime = time_dist1.ProbLess(time_dist2)
    p_adjust = time_dist1.ProbEqual(time_dist2)
    p_win_in_overtime += p_adjust / 2
    print("p_win_in_overtime", p_win_in_overtime)

    # Finally, we can compute the overall chance that the Bruins win, either in regulation or overtime.

    p_win_overall = p_win + p_tie * p_win_in_overtime
    print("p_win_overall", p_win_overall)


def overtime2_strategy():
    # To make the model of overtime more correct,
    # we could update both suites with 0 goals in one game,
    # before computing the predictive distribution of `t`.
    # Make this change and see what effect it has on the results.

    suite1 = Hockey2("bruins")
    suite2 = Hockey2("canucks")
    suite1.UpdateSet([0, 2, 8, 4])
    suite2.UpdateSet([1, 3, 1, 0])
    goal_dist1 = MakeGoalPmf2(suite1)
    goal_dist2 = MakeGoalPmf2(suite2)
    diff = goal_dist1 - goal_dist2
    p_win = diff.ProbGreater(0)
    p_loss = diff.ProbLess(0)
    p_tie = diff.Prob(0)
    time_dist1 = MakeGoalTimePmf2(suite1)
    time_dist2 = MakeGoalTimePmf2(suite2)
    suite1.Update(0)
    suite2.Update(0)
    time_dist1 = MakeGoalTimePmf2(suite1)
    time_dist2 = MakeGoalTimePmf2(suite2)
    p_win_in_overtime = time_dist1.ProbLess(time_dist2)
    p_adjust = time_dist1.ProbEqual(time_dist2)
    p_win_in_overtime += p_adjust / 2
    print("p_win_in_overtime", p_win_in_overtime)
    p_win_overall = p_win + p_tie * p_win_in_overtime
    print("p_win_overall", p_win_overall)


def soccer_strategy():
    # In the final match of the 2014 FIFA World Cup,
    # Germany defeated Argentina 1-0.
    # What is the probability that Germany had the better team?
    # What is the probability that Germany would win a rematch?
    # For a prior distribution on the goal-scoring rate for each team, use a gamma distribution with parameter 1.3.
    xs = np.linspace(0, 8, 101)
    pmf = MakeGammaPmf(xs, 1.3)
    pmf.Mean()
