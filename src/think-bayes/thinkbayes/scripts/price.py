"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2013 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import csv
import logging

import matplotlib.pyplot as pyplot
import numpy as np
import thinkbayes
from thinkbayes import thinkplot

FORMATS = ["png", "pdf", "eps"]


def read_data(filename="showcases.2011.csv"):
    """Reads a CSV file of data.

    Args:
      filename: string filename

    Returns: sequence of (price1 price2 bid1 bid2 diff1 diff2) tuples
    """
    fp = open(filename)
    reader = csv.reader(fp)
    res = []

    for t in reader:
        _heading = t[0]
        data = t[1:]
        try:
            data = [int(x) for x in data]
            # print heading, data[0], len(data)
            res.append(data)
        except ValueError:
            pass

    fp.close()
    return list(zip(*res))


class Price(thinkbayes.Suite):
    """Represents hypotheses about the price of a showcase."""

    def __init__(self, pmf, player, label=None):
        """Constructs the suite.

        pmf: prior distribution of price
        player: Player object
        label: string
        """
        thinkbayes.Suite.__init__(self, pmf, label=label)
        self.player = player

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: actual price
        data: the contestant's guess
        """
        price = hypo
        guess = data

        error = price - guess
        like = self.player.error_density(error)

        return like


class GainCalculator(object):
    """Encapsulates computation of expected gain."""

    def __init__(self, player, opponent):
        """Constructs the calculator.

        player: Player
        opponent: Player
        """
        self.player = player
        self.opponent = opponent

    def expected_gains(self, low=0, high=75000, n=101):
        """Computes expected gains for a range of bids.

        low: low bid
        high: high bid
        n: number of bids to evaluates

        returns: tuple (sequence of bids, sequence of gains)
    
        """
        bids = np.linspace(low, high, n)

        gains = [self.expected_gain(bid) for bid in bids]

        return bids, gains

    def expected_gain(self, bid):
        """Computes the expected return of a given bid.

        bid: your bid
        """
        suite = self.player.posterior
        total = 0
        for price, prob in sorted(suite.items()):
            gain = self.gain(bid, price)
            total += prob * gain
        return total

    def gain(self, bid, price):
        """Computes the return of a bid, given the actual price.

        bid: number
        price: actual price
        """
        # if you overbid, you get nothing
        if bid > price:
            return 0

        # otherwise compute the probability of winning
        diff = price - bid
        prob = self.prob_win(diff)

        # if you are within 250 dollars, you win both showcases
        if diff <= 250:
            return 2 * price * prob
        else:
            return price * prob

    def prob_win(self, diff):
        """Computes the probability of winning for a given diff.

        diff: how much your bid was off by
        """
        prob = self.opponent.prob_overbid() + self.opponent.prob_worse_than(diff)
        return prob


class Player(object):
    """Represents a player on The Price is Right."""

    n = 101
    price_xs = np.linspace(0, 75000, n)

    def __init__(self, prices, bids, diffs):
        """Construct the Player.

        prices: sequence of prices
        bids: sequence of bids
        diffs: sequence of underness (negative means over)
        """
        logging.debug("%r", f"prices={prices}")
        logging.debug("%r", f"bids={bids}")
        logging.debug("%r", f"diffs={diffs}")

        self.pdf_price = thinkbayes.EstimatedPdf(prices)
        self.cdf_diff = thinkbayes.make_cdf_from_list(diffs)

        mu = 0
        sigma = np.std(diffs)
        self.pdf_error = thinkbayes.NormalPdf(mu, sigma)
        self.prior = None
        self.posterior = None

    def error_density(self, error):
        """Density of the given error in the distribution of error.

        error: how much the bid is under the actual price
        """
        return self.pdf_error.density(error)

    def pmf_price(self):
        """Returns a new Pmf of prices.

        A discrete version of the estimated Pdf.
        """
        return self.pdf_price.make_pmf(xs=self.price_xs)

    def cdf_diff(self):
        """Returns a reference to the Cdf of differences (underness).
        """
        return self.cdf_diff

    def prob_overbid(self):
        """Returns the probability this player overbids.
        """
        return self.cdf_diff.prob(-1)

    def prob_worse_than(self, diff):
        """Probability this player's diff is greater than the given diff.

        diff: how much the oppenent is off by (always positive)
        """
        return 1 - self.cdf_diff.prob(diff)

    def make_beliefs(self, guess):
        """Makes a posterior distribution based on estimated price.

        Sets attributes prior and posterior.

        guess: what the player thinks the showcase is worth        
        """
        pmf = self.pmf_price()
        self.prior = Price(pmf, self, label="prior")
        self.posterior = self.prior.copy(label="posterior")
        self.posterior.update(guess)

    def optimal_bid(self, guess, opponent):
        """Computes the bid that maximizes expected return.
        
        guess: what the player thinks the showcase is worth 
        opponent: Player

        Returns: (optimal bid, expected gain)
        """
        self.make_beliefs(guess)
        calc = GainCalculator(self, opponent)
        bids, gains = calc.expected_gains()
        gain, bid = max(zip(gains, bids))
        return bid, gain

    def plot_beliefs(self, root):
        """Plots prior and posterior beliefs.

        root: string filename root for saved figure
        """
        thinkplot.clear_figure()
        thinkplot.pre_plot(num=2)
        thinkplot.plot_pdfs([self.prior, self.posterior])
        thinkplot.save_plot(root=root, xlabel="price ($)", ylabel="PMF", formats=FORMATS)


def make_plots(player1, player2):
    """Generates two plots.

    price1 shows the priors for the two players
    price2 shows the distribution of diff for the two players
    """

    # plot the prior distribution of price for both players
    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)
    pmf1 = player1.pmf_price()
    pmf1.label = "showcase 1"
    pmf2 = player2.pmf_price()
    pmf2.label = "showcase 2"
    thinkplot.plot_pdfs([pmf1, pmf2])
    thinkplot.save_plot(root="price1", xlabel="price ($)", ylabel="PDF", formats=FORMATS)

    # plot the historical distribution of underness for both players
    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)
    cdf1 = player1.cdf_diff()
    cdf1.label = "player 1"
    cdf2 = player2.cdf_diff()
    cdf2.label = "player 2"

    print("Player median", cdf1.percentile(50))
    print("Player median", cdf2.percentile(50))

    print("Player 1 overbids", player1.prob_overbid())
    print("Player 2 overbids", player2.prob_overbid())

    thinkplot.plot_cdfs([cdf1, cdf2])
    thinkplot.save_plot(root="price2", xlabel="diff ($)", ylabel="CDF", formats=FORMATS)


def make_players():
    """Reads data and makes player objects."""
    data = read_data(filename="showcases.2011.csv")
    data += read_data(filename="showcases.2012.csv")

    cols = zip(*data)
    price1, price2, bid1, bid2, diff1, diff2 = cols

    # print(list(sorted(price1)))
    # print(len(price1))

    player1 = Player(price1, bid1, diff1)
    player2 = Player(price2, bid2, diff2)

    return player1, player2


def plot_expected_gains(guess1=20000, guess2=40000):
    """Plots expected gains as a function of bid.

    guess1: player1's estimate of the price of showcase 1
    guess2: player2's estimate of the price of showcase 2
    """
    player1, player2 = make_players()
    make_plots(player1, player2)

    player1.make_beliefs(guess1)
    player2.make_beliefs(guess2)

    print("Player 1 prior mle", player1.prior.MaximumLikelihood())
    print("Player 2 prior mle", player2.prior.MaximumLikelihood())
    print("Player 1 mean", player1.posterior.mean())
    print("Player 2 mean", player2.posterior.mean())
    print("Player 1 mle", player1.posterior.MaximumLikelihood())
    print("Player 2 mle", player2.posterior.MaximumLikelihood())

    player1.plot_beliefs("price3")
    player2.plot_beliefs("price4")

    calc1 = GainCalculator(player1, player2)
    calc2 = GainCalculator(player2, player1)

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=2)

    bids, gains = calc1.expected_gains()
    thinkplot.plot_line(bids, gains, label="Player 1")
    print("Player 1 optimal bid", max(zip(gains, bids)))

    bids, gains = calc2.expected_gains()
    thinkplot.plot_line(bids, gains, label="Player 2")
    print("Player 2 optimal bid", max(zip(gains, bids)))

    thinkplot.save_plot(
        root="price5", xlabel="bid ($)", ylabel="expected gain ($)", formats=FORMATS
    )


def plot_optimal_bid():
    """Plots optimal bid vs estimated price.
    """
    player1, player2 = make_players()
    guesses = np.linspace(15000, 60000, 21)

    res = []
    for guess in guesses:
        player1.make_beliefs(guess)

        mean = player1.posterior.mean()
        mle = player1.posterior.MaximumLikelihood()

        calc = GainCalculator(player1, player2)
        bids, gains = calc.expected_gains()
        gain, bid = max(zip(gains, bids))

        res.append((guess, mean, mle, gain, bid))

    guesses, means, _mles, gains, bids = zip(*res)

    thinkplot.pre_plot(num=3)
    pyplot.plot([15000, 60000], [15000, 60000], color="gray")
    thinkplot.plot_line(guesses, means, label="mean")
    # thinkplot.Plot(guesses, mles, label='MLE')
    thinkplot.plot_line(guesses, bids, label="bid")
    thinkplot.plot_line(guesses, gains, label="gain")
    thinkplot.save_plot(root="price6", xlabel="guessed price ($)", formats=FORMATS)


def test_code(calc):
    """Check some intermediate results.

    calc: GainCalculator
    """
    # test ProbWin
    for diff in [0, 100, 1000, 10000, 20000]:
        print(diff, calc.prob_win(diff))
    print()

    # test Return
    price = 20000
    for bid in [17000, 18000, 19000, 19500, 19800, 20001]:
        print(bid, calc.gain(bid, price))
    print()


def main():
    plot_expected_gains()
    plot_optimal_bid()


if __name__ == "__main__":
    main()
