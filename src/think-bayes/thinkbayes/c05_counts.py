def Odds(p):
    """
    converts from probabilities to odds

    :param p:
    :return:
    """
    return p / (1 - p)


def Probability(o):
    """
    converts from odds to probabilities.

    :param o:
    :return:
    """
    return o / (o + 1)
