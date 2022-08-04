from scipy.stats import binom


def strength(actual, expected):
    return binom.cdf(actual, expected, 0.5)


def strength_strategy(self, body: dict):  # noqa: E501
    """ signal strength """
    actual = body["actual"]
    expected = body["expected"]
    _strength = strength(actual, expected)
    out_dict = dict(
        actual=actual,
        expected=expected,
        strength=_strength,
        status_code="200"
    )
    self.publish(out_dict)
