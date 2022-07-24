def values(series):
    return series.value_counts().sort_index()


def prob(A):
    """Probability of A"""
    return A.mean()


def count(A):
    """Number of instances of A"""
    return A.sum()


def conditional(A, B):
    """Conditional probability of A given B"""
    return prob(A[B])


def conjunction(A, B):
    """Probability of both A and B"""
    return prob(A) * conditional(A=B, B=A)


def bayes_theorem(A, B):
    """Conditional probability of A given B, using Bayes's theorem"""
    return prob(A) * conditional(A=B, B=A) / prob(B)
