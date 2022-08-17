"""
Think Bayes: Chapter 5
This notebook presents code and exercises from Think Bayes, second edition.
This is based on code and exercises from Think Bayes: Chapter 5 second edition.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import logging

from thinkbayes.c05_counts import Odds, Probability


def probability_to_odds_strategy(self, body: dict):
    # If 20% of bettors think my horse will win, that corresponds to odds of 1:4, or 0.25.
    result = Odds(body)
    self.publish(result)


def odds_to_probability_strategy(self, body: dict):
    # If the odds against my horse are 1:5, that corresponds to a probability of 1/6.
    occurance = body['occurance']
    nonoccurance = body['nonoccurance']
    result = Probability(occurance / nonoccurance)
    self.publish(result)


def cookie_likelihood_strategy(self, body: dict):
    """
    We can use the odds form of Bayes's theorem to solve the cookie problem:
    Suppose there are two bowls of cookies.
    Bowl 1 contains 30 vanilla cookies and 10 chocolate cookies.
    Bowl 2 contains 20 of each. Now suppose you choose one of the bowls at random and,
    without looking, select a cookie at random.
    The cookie is vanilla.
    What is the probability that it came from Bowl 1?
    """
    observations = body['observations']  # ['vanilla','chocolate']
    prior_odds = 1
    likelihood_ratio = 1
    post_odds = prior_odds * likelihood_ratio
    post_prob = Probability(post_odds)
    logging.info("%r", f"post_odds = {post_odds}")
    logging.info("%r", f"post_prob = {post_prob}")
    bowl1_vanilla = 30
    bowl1_chocolate = 10
    bowl2_vanilla = 20
    bowl2_chocolate = 20
    bowl1_total = bowl1_vanilla + bowl1_chocolate  # 40
    bowl2_total = bowl2_vanilla + bowl2_chocolate  # 40
    vanilla_given_bowl1 = bowl1_vanilla / bowl1_total  # 0.75
    chocolate_given_bowl1 = bowl1_chocolate / bowl1_total  # 0.5
    vanilla_given_bowl2 = bowl2_vanilla / bowl2_total  # 0.25
    chocolate_given_bowl2 = bowl2_chocolate / bowl2_total  # 0.5
    for observation in observations:
        if observation == 'vanilla':
            likelihood_ratio = vanilla_given_bowl1 / vanilla_given_bowl2
        if observation == 'chocolate':
            likelihood_ratio = chocolate_given_bowl1 / chocolate_given_bowl2
        post_odds *= likelihood_ratio  # update
        post_prob = Probability(post_odds)
        logging.info("%r", f"post_odds = {post_odds}")
        logging.info("%r", f"post_prob = {post_prob}")
    result = dict(
        bowl1_odds=post_odds,
        bowl1_prob=post_prob
    )
    self.publish(result)


def oliver_blood_strategy(self, body: dict):
    """
    Oliver's blood
    The likelihood ratio is also useful for talking about the strength of evidence
    without getting bogged down talking about priors.
    As an example,
    we'll solve this problem from MacKay's **Information Theory, Inference, and Learning Algorithms**:
    > Two people have left traces of their own blood at the scene of a crime.
    A suspect, Oliver, is tested and found to have type 'O' blood.
    The blood groups of the two traces are found to be of type 'O'
    (a common type in the local population, having frequency 60)
    and of type 'AB' (a rare type, with frequency 1).
    Do these data [the traces found at the scene] give evidence
    in favor of the proposition that Oliver was one of the people [who left blood at the scene]?
    If Oliver is one of the people who left blood at the crime scene,
    then he accounts for the "O" sample, so the probability of the data
    is just the probability that a random member of the population has type 'AB' blood, which is 1%.
    If Oliver did not leave blood at the scene,
    then we have two samples to account for.
    If we choose two random people from the population,
    what is the chance of finding one with type 'O' and one with type 'AB'?
    Well, there are two ways it might happen:
    the first person we choose might have type 'O' and the second 'AB', or the other way around.
    So the total probability is $2 (0.6) (0.01) = 1.2$%.
    So the likelihood ratio is:
    """
    o_prevalence = 60
    ab_prevalence = 1
    basis = 100
    observations = body["observations"]
    prior_odds = 1  # the prior odds were 1 (that is, 50% probability)
    post_odds = prior_odds
    like1 = ab_prevalence / basis  # type 'AB' blood
    like2 = 2 * o_prevalence / basis * ab_prevalence / basis  # type 'O' blood
    for observation in observations:
        likelihood_ratio = 1
        if observation == 'AB':
            likelihood_ratio = like1 / like2
        if observation == 'O':
            likelihood_ratio = like2 / like1
        post_odds = post_odds * like1 / like2  # the posterior odds would be 0.83
        favor_against = "against" if post_odds < 1 else "in-favor"  # Since the ratio is less than 1,
        logging.info("%r", f"post_odds = {post_odds} {favor_against}")
    post_prob = Probability(post_odds)  # which corresponds to a probability
    result = dict(
        oliver_odds=post_odds,
        oliver_prob=post_prob
    )
    self.publish(result)
