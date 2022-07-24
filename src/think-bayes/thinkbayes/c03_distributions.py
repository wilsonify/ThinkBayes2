from thinkbayes.scripts.train import Train


def Mean(suite):
    total = 0
    for hypo, prob in suite.Items():
        total += hypo * prob
    return total


def MakePosterior(high, dataset, constructor=Train):
    """Solves the train problem.

    Sensitivity to the prior
    Here's a function that solves the train problem for different priors and data


    high: int maximum number of trains
    dataset: sequence of observed train numbers
    constructor: function used to construct the Train object

    returns: Train object representing the posterior suite
    """
    hypos = range(1, high + 1)
    suite = constructor(hypos)

    for data in dataset:
        suite.Update(data)

    return suite


class Train2(Train):
    # The results are quite sensitive to the prior, even with several observations.
    # ## Power law prior
    # Now let's try it with a power law prior.

    def __init__(self, hypos, alpha=1.0):
        super().__init__(self)
        for hypo in hypos:
            self[hypo] = hypo ** (-alpha)
        self.Normalize()
