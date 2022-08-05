def values(series):
    return series.value_counts().sort_index()


def prob(a):
    """Probability of A"""
    return a.mean()


def count(a):
    """Number of instances of A"""
    return a.sum()


def conditional(a, b):
    """Conditional probability of A given B"""
    return prob(a[b])


def conjunction(a, b):
    """Probability of both A and B"""
    return prob(a) * conditional(a=b, b=a)


def bayes_theorem(a, b):
    """Conditional probability of A given B, using Bayes's theorem"""
    return prob(a) * conditional(a=b, b=a) / prob(b)
