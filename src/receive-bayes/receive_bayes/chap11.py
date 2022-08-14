"""
This notebook presents code and exercises from Think Bayes: Chapter 11
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

from thinkbayes.c11_comparison import Euro, SuiteLikelihood, TrianglePrior


def euro_hierarchical_strategy(self, body: dict):
    """
    If we know the coin is fair, we can evaluate the likelihood of the data directly.
    If we cheat and pretend that the alternative hypothesis is exactly the observed proportion,
    we can compute the likelihood of the data and the likelihood ratio, relative to the fair coin.

    Under this interpretation, the data are in favor of "biased", with K=6.
    But that's a total cheat.

    Suppose we think "biased" means either 0.4 or 0.6, but we're not sure which.
    The total likelihood of the data is the weighted average of the two likelihoods.

    Here's what it looks like if "biased" means "equally likely to be any value between 0 and 1".
    By the triangle definition of "biased", the data are very weakly in favor of "fair".

    We don't really need the SuiteLikelihood function, because `Suite.Update`
    already computes the total probability of the data, which is the normalizing constant.
    This observation is the basis of hierarchical Bayesian models, this solution is a simple example.
    :return:
    """
    data = body["data"]
    suite = Euro()
    like_f = suite.Likelihood(data, 50)
    actual_percent = 100.0 * 140 / 250
    likelihood = suite.Likelihood(data, actual_percent)
    like40 = suite.Likelihood(data, 40)
    like60 = suite.Likelihood(data, 60)
    likelihood_updated = 0.5 * like40 + 0.5 * like60
    b_uniform = Euro(range(0, 101))
    b_uniform.Remove(50)
    b_uniform.Normalize()
    likelihood_updated_again = SuiteLikelihood(b_uniform, data)
    b_tri = TrianglePrior()
    b_tri.Remove(50)
    b_tri.Normalize()
    likelihood_updated_a_third_time = b_tri.Update(data)
    likelihood_updated_a_fourth_time = SuiteLikelihood(b_uniform, data)
    logging.info("%r", f"likelihood_updated_a_fourth_time = {likelihood_updated_a_fourth_time}")
    euro = Euro(b_uniform)
    euro.Update(data)
    likelihood_final = SuiteLikelihood(b_tri, data)
    logging.info("%r", f"likelihood_final = {likelihood_final}")
    euro = Euro(b_tri)
    euro.Update(data)
    result = euro.Mean()
    self.publish(result)
    result = {
        "p(D|F)": like_f,
        "p(D|B_cheat)": likelihood,
        "p(D|B_cheat) / p(D|F)": likelihood / like_f,
        "p(D|B_two)": likelihood_updated,
        "p(D|B_two) / p(D|F)": likelihood_updated / like_f,
        "p(D|B_uniform)": likelihood_updated_again,
        "p(D|B_uniform) / p(D|F)": likelihood_updated_again / like_f,
        "p(D|B_tri)": likelihood_updated_a_third_time,
        "p(D|B_tri) / p(D|F)": likelihood_updated_a_third_time / like_f,
    }
    self.publish(result)


def euro_hierarchical_cheat_strategy(self, body: dict):
    """
    If we know the coin is fair, we can evaluate the likelihood of the data directly.
    If we cheat and pretend that the alternative hypothesis is exactly the observed proportion,
    we can compute the likelihood of the data and the likelihood ratio, relative to the fair coin.
    Under this interpretation, the data are in favor of "biased", with K=6.
    But that's a total cheat.
    """
    data = body["data"]
    suite = Euro()
    like_f = suite.Likelihood(data, 50)
    actual_percent = 100.0 * 140 / 250
    likelihood = suite.Likelihood(data, actual_percent)
    like40 = suite.Likelihood(data, 40)
    like60 = suite.Likelihood(data, 60)
    likelihood_updated = 0.5 * like40 + 0.5 * like60
    result = {
        "p(D|F)": like_f,
        "p(D|B_cheat)": likelihood,
        "p(D|B_cheat) / p(D|F)": likelihood / like_f,
        "p(D|B_two)": likelihood_updated,
        "p(D|B_two) / p(D|F)": likelihood_updated / like_f
    }
    self.publish(result)


def euro_hierarchical_uniform_strategy(self, body: dict):
    """
    If we know the coin is fair, we can evaluate the likelihood of the data directly.
    If we cheat and pretend that the alternative hypothesis is exactly the observed proportion,
    we can compute the likelihood of the data and the likelihood ratio, relative to the fair coin.

    Under this interpretation, the data are in favor of "biased", with K=6.
    But that's a total cheat.

    Suppose we think "biased" means either 0.4 or 0.6, but we're not sure which.
    The total likelihood of the data is the weighted average of the two likelihoods.

    Here's what it looks like if "biased" means "equally likely to be any value between 0 and 1".
    By the triangle definition of "biased", the data are very weakly in favor of "fair".

    We don't really need the SuiteLikelihood function, because `Suite.Update`
    already computes the total probability of the data, which is the normalizing constant.
    This observation is the basis of hierarchical Bayesian models, this solution is a simple example.
    :return:
    """
    data = body["data"]
    suite = Euro()
    like_f = suite.Likelihood(data, 50)
    actual_percent = 100.0 * 140 / 250
    likelihood = suite.Likelihood(data, actual_percent)
    like40 = suite.Likelihood(data, 40)
    like60 = suite.Likelihood(data, 60)
    likelihood_updated = 0.5 * like40 + 0.5 * like60
    b_uniform = Euro(range(0, 101))
    b_uniform.Remove(50)
    b_uniform.Normalize()
    likelihood_updated_again = SuiteLikelihood(b_uniform, data)
    result = {
        "p(D|F)": like_f,
        "p(D|B_cheat)": likelihood,
        "p(D|B_cheat) / p(D|F)": likelihood / like_f,
        "p(D|B_two)": likelihood_updated,
        "p(D|B_two) / p(D|F)": likelihood_updated / like_f,
        "p(D|B_uniform)": likelihood_updated_again,
        "p(D|B_uniform) / p(D|F)": likelihood_updated_again / like_f
    }
    self.publish(result)
