"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging

from thinkbayes import Suite

from thinkbayes import thinkplot as tplt


def test_urns():
    # Here's a problem from Joyce, "[How probabilities reflect evidence](http://www-personal.umich.edu/~jjoyce/papers/hpre.pdf)":
    #
    # >Four Urns: Jacob and Emily both start out knowing that the urn U was
    # randomly chosen from a set of four urns {urn0, urn1, urn2, urn3} where urn_i
    # contains three balls, i of which are blue and 3-i of which are green. Since the
    # choice of U was random both subjects assign equal credence to the four
    # hypotheses about its contents: c(U = urn_i) = 1/4. Moreover, both treat these
    # hypotheses as statements about the objective chance of drawing a blue ball from
    # U, so that knowledge of U = urn_i ‘screen offs’ any sampling data in the sense
    # that c(Bnext |E & U = urn_i) = c(Bnext | U = urni), where Bnext says that the next
    # ball drawn from the urn will be blue and E is a proposition that describes any
    # prior series of random draws with replacement from U. Finally, Jacob and
    # Emily regard random drawing with replacement as an exchangeable process,
    # so that any series of draws that produces m blue balls and n green balls is as
    # likely as any other such series, irrespective of order. Use BmGn to denote the
    # generic event in which m blue balls and n green balls are drawn at random and
    # with replacement form U. Against this backdrop of shared evidence, suppose
    # Jacob sees five balls drawn at random and with replacement from U and
    # observes that all are blue, so his evidence is B5G0. Emily, who sees Jacob’s
    # evidence, looks at fifteen additional draws of which twelve come up blue, so her
    # evidence is B17G3. What should Emily and Jacob think about Bnext?
    #
    # Here's a class that represents a suite of hypotheses about the urns:

    class Urns(Suite):
        def Likelihood(self, data, hypo):
            """Computes the likelihood of the data under the hypothesis.

            data: 'B' or 'G'
            hypo: urn index from 0..3
            """
            prob_blue = hypo / 3
            if data == "B":
                return prob_blue
            else:
                return 1 - prob_blue

    # Here's the uniform prior:

    prior = Urns([0, 1, 2, 3])
    tplt.plot_hist_bar(prior)
    tplt.decorate(xlabel="Urn index (i)", ylabel="PMF")

    # Here's Jacob's update after 5 blue marbles.

    jacob = prior.Copy()
    B5G0 = "B" * 5

    for data in B5G0:
        jacob.update(data)

    jacob.Print()

    tplt.plot_hist_bar(prior, color="gray")
    tplt.plot_hist_bar(jacob)
    tplt.decorate(xlabel="Urn index (i)", ylabel="PMF")

    # Here's Emily's update after an additional 12 blue and 3 green.

    emily = jacob.Copy()
    B12G3 = "B" * 12 + "G" * 3

    for data in B12G3:
        emily.update(data)

    emily.Print()

    tplt.preplot(cols=2)
    tplt.plot_hist_bar(jacob, label="Jacob")
    tplt.decorate(xlabel="Urn index (i)", ylabel="PMF")

    tplt.subplot(2)
    tplt.plot_hist_bar(emily, label="Emily")
    tplt.decorate(xlabel="Urn index (i)", ylabel="PMF")

    # What should Jacob believe about Bnext?

    total = 0
    for i, prob_i in jacob.Items():
        print(i, prob_i)
        prob_blue = i / 3
        total += prob_i * prob_blue

    logging.info("%r", f"total = {total}")


    # Let's make it a function:

    def prob_b_next(suite):
        total = 0
        for i, prob_i in suite.Items():
            prob_blue = i / 3
            total += prob_i * prob_blue

        return total

    # Here's what Jacob believes.

    prob_b_next(jacob)

    # And Emily.

    prob_b_next(emily)

    # Suppose we draw a **blue** marble from the same urn and show it to Jacob and Emily.  How much do their beliefs about Bnext change?
    #
    # Here's the effect on Jacob.

    print(prob_b_next(jacob))
    jacob.update("B")
    print(prob_b_next(jacob))

    # And on Emily.

    print(prob_b_next(emily))
    emily.update("B")
    print(prob_b_next(emily))

    # Suppose we draw a **green** marble from the same urn and show it to Jacob and Emily.  How much do their beliefs about Bnext change?

    print(prob_b_next(jacob))
    jacob.update("G")
    print(prob_b_next(jacob))

    print(prob_b_next(emily))
    emily.update("G")
    print(prob_b_next(emily))
