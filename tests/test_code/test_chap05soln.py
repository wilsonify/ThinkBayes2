"""
Think Bayes: Chapter 5
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging


def test_chapt5():
    # ## Odds
    #
    # The following function converts from probabilities to odds.

    def Odds(p):
        return p / (1 - p)

    # And this function converts from odds to probabilities.

    def Probability(o):
        return o / (o + 1)

    # If 20% of bettors think my horse will win, that corresponds to odds of 1:4, or 0.25.

    p = 0.2
    Odds(p)

    # If the odds against my horse are 1:5, that corresponds to a probability of 1/6.

    o = 1 / 5
    Probability(o)

    # We can use the odds form of Bayes's theorem to solve the cookie problem:

    prior_odds = 1
    likelihood_ratio = 0.75 / 0.5
    post_odds = prior_odds * likelihood_ratio
    logging.info("%r", f"post_odds = {post_odds}")


    # And then we can compute the posterior probability, if desired.

    post_prob = Probability(post_odds)
    logging.info("%r", f"post_prob = {post_prob}")


    # If we draw another cookie and it's chocolate, we can do another update:

    likelihood_ratio = 0.25 / 0.5
    post_odds *= likelihood_ratio
    logging.info("%r", f"post_odds = {post_odds}")


    # And convert back to probability.

    post_prob = Probability(post_odds)
    logging.info("%r", f"post_prob = {post_prob}")


    # ## Oliver's blood
    #
    # The likelihood ratio is also useful for talking about the strength of evidence without getting bogged down talking about priors.
    #
    # As an example, we'll solve this problem from MacKay's {\it Information Theory, Inference, and Learning Algorithms}:
    #
    # > Two people have left traces of their own blood at the scene of a crime.  A suspect, Oliver, is tested and found to have type 'O' blood.  The blood groups of the two traces are found to be of type 'O' (a common type in the local population, having frequency 60) and of type 'AB' (a rare type, with frequency 1). Do these data [the traces found at the scene] give evidence in favor of the proposition that Oliver was one of the people [who left blood at the scene]?
    #
    # If Oliver is
    # one of the people who left blood at the crime scene, then he
    # accounts for the 'O' sample, so the probability of the data
    # is just the probability that a random member of the population
    # has type 'AB' blood, which is 1%.
    #
    # If Oliver did not leave blood at the scene, then we have two
    # samples to account for.  If we choose two random people from
    # the population, what is the chance of finding one with type 'O'
    # and one with type 'AB'?  Well, there are two ways it might happen:
    # the first person we choose might have type 'O' and the second
    # 'AB', or the other way around.  So the total probability is
    # $2 (0.6) (0.01) = 1.2$%.
    #
    # So the likelihood ratio is:

    like1 = 0.01
    like2 = 2 * 0.6 * 0.01

    likelihood_ratio = like1 / like2
    logging.info("%r", f"likelihood_ratio = {likelihood_ratio}")


    # Since the ratio is less than 1, it is evidence *against* the hypothesis that Oliver left blood at the scence.
    #
    # But it is weak evidence.  For example, if the prior odds were 1 (that is, 50% probability), the posterior odds would be 0.83, which corresponds to a probability of:

    post_odds = 1 * like1 / like2
    Probability(post_odds)

    # So this evidence doesn't "move the needle" very much.

    # **Exercise:** Suppose other evidence had made you 90% confident of Oliver's guilt.  How much would this exculpatory evidence change your beliefs?  What if you initially thought there was only a 10% chance of his guilt?
    #
    # Notice that evidence with the same strength has a different effect on probability, depending on where you started.

    # Solution

    post_odds = Odds(0.9) * like1 / like2
    Probability(post_odds)

    # Solution

    post_odds = Odds(0.1) * like1 / like2
    Probability(post_odds)
