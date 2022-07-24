from scipy.stats import binom


def strength(actual, expected):
    binom.cdf(actual, expected, 0.5)


def mystrength_strategy(self, body: dict):  # noqa: E501
    """ signal strength """
    actual = body["actual"]
    expected = body["expected"]
    strength = strength(actual, expected)
    out_dict = dict(
        actual=actual,
        expected=expected,
        strength=strength,
        status_code="200"
    )
    self.publish(out_dict)
