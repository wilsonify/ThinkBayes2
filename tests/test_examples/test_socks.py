import thinkbayes
from thinkbayes import thinkplot


class Socks(thinkbayes.Suite):
    def __init__(self, hypos):
        """Inits a Sock object.
        
        hypos: sequence or map of number of pairs in the drawer
        
        matched: number of matched socks that have been picked
        unmatched: number of unmatched socks that have been picked
        """
        thinkbayes.Suite.__init__(self, hypos)
        self.matched = 0
        self.unmatched = 0

    def update(self, data):
        """Updates the suite based on data.
        
        data: 'u' if we picked an unmatched sock, 'm' otherwise
        """
        thinkbayes.Suite.update(self, data)
        if data == "u":
            self.unmatched += 1
        else:
            self.matched += 1

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.
        
        data: 'u' if we picked an unmatched sock, 'm' otherwise
        hypo: hypothetical number of pairs
        """
        n_pairs = hypo
        n_socks = 2 * n_pairs - self.matched - self.unmatched
        if n_socks <= 0:
            return 0

        n_singletons = self.unmatched - self.matched
        p = n_singletons / n_socks

        like = 1 - p if data == "u" else p
        return like


def test_socks():
    prior_n_pairs = thinkbayes.make_poisson_pmf(12, 30)
    suite = Socks(prior_n_pairs)
    thinkplot.plot_hist_bar(suite)
    thinkplot.config_plot(xlabel="# pairs", ylabel="PMF", xlim=[0, 30])

    hypos = range(1, 1001)
    suite = Socks(hypos)
    for datum in "u" * 11:
        suite.update("u")
    thinkplot.plot_hist_bar(suite)
    thinkplot.config_plot(xlabel="# pairs", ylabel="PMF", xlim=[0, 30])

    class Socks2(Socks, thinkbayes.Joint):
        def likelihood(self, data, hypo):
            """Computes the likelihood of the data under the hypothesis.

            data: 'u' if we picked an unmatched sock, 'm' otherwise
            hypo: hypothetical number of pairs, number of odds
            """
            n_pairs, n_odds = hypo
            n_socks = 2 * n_pairs + n_odds - self.matched - self.unmatched
            if n_socks <= 0:
                return 0

            n_singletons = self.unmatched - self.matched
            p = n_singletons / n_socks

            like = 1 - p if data == "u" else p
            return like

    prior_n_odds = thinkbayes.make_poisson_pmf(3, 30)
    thinkplot.plot_hist_bar(prior_n_odds)
    thinkplot.config_plot(xlabel="# odds", ylabel="PMF", xlim=[0, 30])

    joint = thinkbayes.make_joint(prior_n_pairs, prior_n_odds)
    suite = Socks2(joint)
    for datum in "u" * 11:
        suite.update("u")

    post_n_pairs = suite.marginal(0)
    thinkplot.plot_hist_bar(post_n_pairs)
    thinkplot.config_plot(xlabel="# pairs", ylabel="PMF", xlim=[0, 30])

    post_n_odds = suite.marginal(1)
    thinkplot.plot_hist_bar(post_n_odds)
    thinkplot.config_plot(xlabel="# odds", ylabel="PMF", xlim=[0, 30])
