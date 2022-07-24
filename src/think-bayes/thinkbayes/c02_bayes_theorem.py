from thinkbayes import Pmf, Suite


class Cookie(Pmf):
    """
    A Bayesian framework
    Here's the same computation encapsulated in a class.
    A map from string bowl ID to probablity.
    """

    def __init__(self, hypos):
        """Initialize self.

        hypos: sequence of string bowl IDs
        """
        Pmf.__init__(self)
        for hypo in hypos:
            self.Set(hypo, 1)
        self.Normalize()

    def Update(self, data):
        """Updates the PMF with new data.

        data: string cookie type
        """
        for hypo in self.Values():
            self[hypo] *= self.Likelihood(data, hypo)
        self.Normalize()

    mixes = {
        "Bowl1": dict(vanilla=0.75, chocolate=0.25),
        "Bowl2": dict(vanilla=0.5, chocolate=0.5),
    }

    def Likelihood(self, data, hypo):
        """The likelihood of the data under the hypothesis.

        data: string cookie type
        hypo: string bowl ID
        """
        mix = self.mixes[hypo]
        like = mix[data]
        return like


class FullMonty(Pmf):
    """
    The Monty Hall problem

     The Monty Hall problem might be the most contentious question in
     the history of probability.  The scenario is simple, but the correct
     answer is so counterintuitive that many people just can't accept
     it, and many smart people have embarrassed themselves not just by
     getting it wrong but by arguing the wrong side, aggressively,
     in public.
     Monty Hall was the original host of the game show *Let's Make a
     Deal*.  The Monty Hall problem is based on one of the regular
     games on the show.  If you are on the show, here's what happens:
     *  Monty shows you three closed doors and tells you that there is a
        prize behind each door: one prize is a car, the other two are less
        valuable prizes like peanut butter and fake finger nails.  The
        prizes are arranged at random.
     *  The object of the game is to guess which door has the car.  If
        you guess right, you get to keep the car.
     *  You pick a door, which we will call Door A.  We'll call the
        other doors B and C.
     *  Before opening the door you chose, Monty increases the
        suspense by opening either Door B or C, whichever does not
        have the car.  (If the car is actually behind Door A, Monty can
        safely open B or C, so he chooses one at random.)
     *  Then Monty offers you the option to stick with your original
        choice or switch to the one remaining unopened door.
     The question is, should you "stick" or "switch" or does it
     make no difference?
     Most people have the strong intuition that it makes no difference.
     There are two doors left, they reason, so the chance that the car
     is behind Door A is 50%.
     But that is wrong.  In fact, the chance of winning if you stick
     with Door A is only 1/3; if you switch, your chances are 2/3.
     Here's a class that solves the Monty Hall problem.

    Map from string location of car to probability

    """

    def __init__(self, hypos):
        """Initialize the distribution.

        hypos: sequence of hypotheses
        """
        Pmf.__init__(self)
        for hypo in hypos:
            self.Set(hypo, 1)
        self.Normalize()

    def Update(self, data):
        """Updates each hypothesis based on the data.

        data: string 'A', 'B', or 'C'
        """
        for hypo in self.Values():
            self[hypo] *= self.Likelihood(data, hypo)
        self.Normalize()

    def Likelihood(self, data, hypo):
        """Compute the likelihood of the data under the hypothesis.

        hypo: string name of the door where the prize is
        data: string name of the door Monty opened
        """
        if hypo == data:
            return 0
        elif hypo == "A":
            return 0.5
        else:
            return 1


class Monty(Suite):
    """
    # ## The Suite class
    #
    # Most Bayesian updates look pretty much the same, especially the `Update` method.
    # So we can encapsulate the framework in a class, `Suite`, and create new classes that extend it.
    #

    # %psource Suite

    # Child classes of `Suite` inherit `Update` and provide `Likelihood`.
    #
    # So here's the short version of `Monty`

    """

    def Likelihood(self, data, hypo):
        if hypo == data:
            return 0
        elif hypo == "A":
            return 0.5
        else:
            return 1


class Cookie2(Pmf):
    """A map from string bowl ID to probablity."""

    def __init__(self, hypos):
        """Initialize self.

        hypos: sequence of string bowl IDs
        """
        Pmf.__init__(self)
        for hypo in hypos:
            self.Set(hypo, 1)
        self.Normalize()

    def Update(self, data):
        """Updates the PMF with new data.

        data: string cookie type
        """
        for hypo in self.Values():
            self[hypo] *= self.Likelihood(data, hypo)
        self.Normalize()

    mixes = {
        "Bowl1": dict(vanilla=0.75, chocolate=0.25),
        "Bowl2": dict(vanilla=0.5, chocolate=0.5),
    }

    def Likelihood(self, data, hypo):
        """The likelihood of the data under the hypothesis.

        data: string cookie type
        hypo: string bowl ID
        """
        mix = self.mixes[hypo]
        like = mix[data]
        return like


class Monty2(Pmf):
    """Map from string location of car to probability"""

    def __init__(self, hypos):
        """Initialize the distribution.

        hypos: sequence of hypotheses
        """
        Pmf.__init__(self)
        for hypo in hypos:
            self.Set(hypo, 1)
        self.Normalize()

    def Update(self, data):
        """Updates each hypothesis based on the data.

        data: string 'A', 'B', or 'C'
        """
        for hypo in self.Values():
            self[hypo] *= self.Likelihood(data, hypo)
        self.Normalize()

    def Likelihood(self, data, hypo):
        """Compute the likelihood of the data under the hypothesis.

        hypo: string name of the door where the prize is
        data: string name of the door Monty opened
        """
        if hypo == data:
            return 0
        elif hypo == "A":
            return 0.5
        else:
            return 1


class Monty3(Suite):
    """
    short version of `Monty`
    Child classes of `Suite` inherit `Update` and provide `Likelihood`.
    """

    def Likelihood(self, data, hypo):
        if hypo == data:
            return 0
        elif hypo == "A":
            return 0.5
        else:
            return 1


class M_and_M2(Suite):
    """Map from hypothesis (A or B) to probability."""

    mix94 = dict(brown=30, yellow=20, red=20, green=10, orange=10, tan=10, blue=0)

    mix96 = dict(blue=24, green=20, orange=16, yellow=14, red=13, brown=13, tan=0)

    hypoA = dict(bag1=mix94, bag2=mix96)
    hypoB = dict(bag1=mix96, bag2=mix94)

    hypotheses = dict(A=hypoA, B=hypoB)

    def Likelihood(self, data, hypo):
        """Computes the likelihood of the data under the hypothesis.

        hypo: string hypothesis (A or B)
        data: tuple of string bag, string color
        """
        bag, color = data
        mix = self.hypotheses[hypo][bag]
        like = mix[color]
        return like
