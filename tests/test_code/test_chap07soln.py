"""
Think Bayes: Chapter 7
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

import numpy as np
from scipy.stats import poisson
from thinkbayes import MakeMixture
from thinkbayes import MakeNormalPmf
from thinkbayes import Pmf, Suite
from thinkbayes import thinkplot


# ## Warm-up exercises
# **Exercise:** Suppose that goal scoring in hockey is well modeled by a
# Poisson process, and that the long-run goal-scoring rate of the
# Boston Bruins against the Vancouver Canucks is 2.9 goals per game.
# In their next game, what is the probability
# that the Bruins score exactly 3 goals?  Plot the PMF of `k`, the number
# of goals they score in a game.
# Solution


def test_chapt7():
    poisson.pmf(3, 2.9)

    # Solution

    from thinkbayes import EvalPoissonPmf

    EvalPoissonPmf(3, 2.9)

    # Solution

    from thinkbayes import MakePoissonPmf

    pmf = MakePoissonPmf(2.9, high=10)
    thinkplot.plot_hist_bar(pmf)
    thinkplot.config_plot(xlabel="Number of goals", ylabel="PMF", xlim=[-0.5, 10.5])

    # **Exercise:**  Assuming again that the goal scoring rate is 2.9, what is the probability of scoring a total of 9 goals in three games?  Answer this question two ways:
    #
    # 1.  Compute the distribution of goals scored in one game and then add it to itself twice to find the distribution of goals scored in 3 games.
    #
    # 2.  Use the Poisson PMF with parameter $\lambda t$, where $\lambda$ is the rate in goals per game and $t$ is the duration in games.

    # Solution

    pmf = MakePoissonPmf(2.9, high=30)
    total = pmf + pmf + pmf
    thinkplot.plot_hist_bar(total)
    thinkplot.config_plot(xlabel="Number of goals", ylabel="PMF", xlim=[-0.5, 22.5])
    logging.info("%r", f"total[9] = {total[9]}")

    # Solution

    EvalPoissonPmf(9, 3 * 2.9)

    # **Exercise:** Suppose that the long-run goal-scoring rate of the
    # Canucks against the Bruins is 2.6 goals per game.  Plot the distribution
    # of `t`, the time until the Canucks score their first goal.
    # In their next game, what is the probability that the Canucks score
    # during the first period (that is, the first third of the game)?
    #
    # Hint: `thinkbayes2` provides `MakeExponentialPmf` and `EvalExponentialCdf`.

    # Solution

    from thinkbayes import MakeExponentialPmf

    pmf = MakeExponentialPmf(lam=2.6, high=2.5)
    thinkplot.plot_pdf_line(pmf)
    thinkplot.config_plot(xlabel="Time between goals", ylabel="PMF")

    # Solution

    from scipy.stats import expon

    expon.cdf(1 / 3, scale=1 / 2.6)

    # Solution

    from thinkbayes import EvalExponentialCdf

    EvalExponentialCdf(1 / 3, 2.6)

    # **Exercise:** Assuming again that the goal scoring rate is 2.8, what is the probability that the Canucks get shut out (that is, don't score for an entire game)?  Answer this question two ways, using the CDF of the exponential distribution and the PMF of the Poisson distribution.

    # Solution

    logging.info("%r", f"1 - EvalExponentialCdf(1, 2.6) = {1 - EvalExponentialCdf(1, 2.6)}")

    # Solution

    EvalPoissonPmf(0, 2.6)

    # ## The Boston Bruins problem
    #
    # The `Hockey` suite contains hypotheses about the goal scoring rate for one team against the other.  The prior is Gaussian, with mean and variance based on previous games in the league.
    #
    # The Likelihood function takes as data the number of goals scored in a game.

    class Hockey(Suite):
        """Represents hypotheses about the scoring rate for a team."""

        def __init__(self, label=None):
            """Initializes the Hockey object.

            label: string
            """
            mu = 2.8
            sigma = 0.3

            pmf = MakeNormalPmf(mu, sigma, num_sigmas=4, n=101)
            Suite.__init__(self, pmf, label=label)

        def Likelihood(self, data, hypo):
            """Computes the likelihood of the data under the hypothesis.

            Evaluates the Poisson PMF for lambda and k.

            hypo: goal scoring rate in goals per game
            data: goals scored in one game
            """
            lam = hypo
            k = data
            like = EvalPoissonPmf(k, lam)
            return like

    # Now we can initialize a suite for each team:

    suite1 = Hockey("bruins")
    suite2 = Hockey("canucks")

    # Here's what the priors look like:

    thinkplot.pre_plot(num=2)
    thinkplot.plot_pdf_line(suite1)
    thinkplot.plot_pdf_line(suite2)
    thinkplot.config_plot(xlabel="Goals per game", ylabel="Probability")

    # And we can update each suite with the scores from the first 4 games.

    suite1.UpdateSet([0, 2, 8, 4])
    suite2.UpdateSet([1, 3, 1, 0])

    thinkplot.pre_plot(num=2)
    thinkplot.plot_pdf_line(suite1)
    thinkplot.plot_pdf_line(suite2)
    thinkplot.config_plot(xlabel="Goals per game", ylabel="Probability")

    logging.info("%r", f"suite1.Mean() = {suite1.Mean()}")
    logging.info("%r", f"suite2.Mean() = {suite2.Mean()}")

    # To predict the number of goals scored in the next game we can compute, for each hypothetical value of $\lambda$, a Poisson distribution of goals scored, then make a weighted mixture of Poissons:

    def MakeGoalPmf(suite, high=10):
        """Makes the distribution of goals scored, given distribution of lam.

        suite: distribution of goal-scoring rate
        high: upper bound

        returns: Pmf of goals per game
        """
        metapmf = Pmf()

        for lam, prob in suite.Items():
            pmf = MakePoissonPmf(lam, high)
            metapmf.Set(pmf, prob)

        mix = MakeMixture(metapmf, label=suite.label)
        return mix

    # Here's what the results look like.

    goal_dist1 = MakeGoalPmf(suite1)
    goal_dist2 = MakeGoalPmf(suite2)

    thinkplot.pre_plot(num=2)
    thinkplot.plot_pmf_line(goal_dist1)
    thinkplot.plot_pmf_line(goal_dist2)
    thinkplot.config_plot(xlabel="Goals", ylabel="Probability", xlim=[-0.7, 11.5])

    logging.info("%r", f"goal_dist1.Mean() = {goal_dist1.Mean()}")
    logging.info("%r", f"goal_dist2.Mean() = {goal_dist2.Mean()}")

    # Now we can compute the probability that the Bruins win, lose, or tie in regulation time.

    diff = goal_dist1 - goal_dist2
    p_win = diff.ProbGreater(0)
    p_loss = diff.ProbLess(0)
    p_tie = diff.Prob(0)

    print("Prob win, loss, tie:", p_win, p_loss, p_tie)

    # If the game goes into overtime, we have to compute the distribution of `t`, the time until the first goal, for each team.  For each hypothetical value of $\lambda$, the distribution of `t` is exponential, so the predictive distribution is a mixture of exponentials.

    def MakeGoalTimePmf(suite):
        """Makes the distribution of time til first goal.

        suite: distribution of goal-scoring rate

        returns: Pmf of goals per game
        """
        metapmf = Pmf()

        for lam, prob in suite.Items():
            pmf = MakeExponentialPmf(lam, high=2.5, n=1001)
            metapmf.Set(pmf, prob)

        mix = MakeMixture(metapmf, label=suite.label)
        return mix

    # Here's what the predictive distributions for `t` look like.

    time_dist1 = MakeGoalTimePmf(suite1)
    time_dist2 = MakeGoalTimePmf(suite2)

    thinkplot.pre_plot(num=2)
    thinkplot.plot_pmf_line(time_dist1)
    thinkplot.plot_pmf_line(time_dist2)
    thinkplot.config_plot(xlabel="Games until goal", ylabel="Probability")

    logging.info("%r", f"time_dist1.Mean() = {time_dist1.Mean()}")
    logging.info("%r", f"time_dist2.Mean() = {time_dist2.Mean()}")

    # In overtime the first team to score wins, so the probability of winning is the probability of generating a smaller value of `t`:

    p_win_in_overtime = time_dist1.ProbLess(time_dist2)
    p_adjust = time_dist1.ProbEqual(time_dist2)
    p_win_in_overtime += p_adjust / 2
    print("p_win_in_overtime", p_win_in_overtime)

    # Finally, we can compute the overall chance that the Bruins win, either in regulation or overtime.

    p_win_overall = p_win + p_tie * p_win_in_overtime
    print("p_win_overall", p_win_overall)

    # ## Exercises

    # **Exercise:** To make the model of overtime more correct, we could update both suites with 0 goals in one game, before computing the predictive distribution of `t`.  Make this change and see what effect it has on the results.

    # Solution

    suite1.Update(0)
    suite2.Update(0)
    time_dist1 = MakeGoalTimePmf(suite1)
    time_dist2 = MakeGoalTimePmf(suite2)
    p_win_in_overtime = time_dist1.ProbLess(time_dist2)
    p_adjust = time_dist1.ProbEqual(time_dist2)
    p_win_in_overtime += p_adjust / 2
    print("p_win_in_overtime", p_win_in_overtime)
    p_win_overall = p_win + p_tie * p_win_in_overtime
    print("p_win_overall", p_win_overall)

    # **Exercise:** In the final match of the 2014 FIFA World Cup, Germany defeated Argentina 1-0. What is the probability that Germany had the better team?  What is the probability that Germany would win a rematch?
    #
    # For a prior distribution on the goal-scoring rate for each team, use a gamma distribution with parameter 1.3.

    from thinkbayes import MakeGammaPmf

    xs = np.linspace(0, 8, 101)
    pmf = MakeGammaPmf(xs, 1.3)
    thinkplot.plot_pdf_line(pmf)
    thinkplot.config_plot(xlabel="Goals per game")
    pmf.Mean()

    # **Exercise:** In the 2014 FIFA World Cup, Germany played Brazil in a semifinal match. Germany scored after 11 minutes and again at the 23 minute mark. At that point in the match, how many goals would you expect Germany to score after 90 minutes? What was the probability that they would score 5 more goals (as, in fact, they did)?
    #
    # Note: for this one you will need a new suite that provides a Likelihood function that takes as data the time between goals, rather than the number of goals in a game.

    # **Exercise:** Which is a better way to break a tie: overtime or penalty shots?

    # **Exercise:** Suppose that you are an ecologist sampling the insect population in a new environment. You deploy 100 traps in a test area and come back the next day to check on them. You find that 37 traps have been triggered, trapping an insect inside. Once a trap triggers, it cannot trap another insect until it has been reset.
    # If you reset the traps and come back in two days, how many traps do you expect to find triggered? Compute a posterior predictive distribution for the number of traps.
