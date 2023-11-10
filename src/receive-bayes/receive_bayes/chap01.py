"""
This notebook presents example code and exercise solutions for Think Bayes.
"""
import logging

import pandas as pd

from thinkbayes.c01_probability import (
    prob,
    conditional,
    conjunction,
    bayes_theorem
)


def prob_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    result = prob(a)
    self.publish(result)


def conjunction_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    b = pd.Series(body["b"]).astype(bool)
    logging.debug(f"a.shape={a.shape}")
    logging.debug(f"b.shape={b.shape}")
    result = conjunction(a, b)
    logging.debug(f"result={result}")
    self.publish(result)


def conditional_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    b = pd.Series(body["b"]).astype(bool)
    result = conditional(a, b)
    self.publish(result)


def bayes_strategy(self, body):
    a = pd.Series(body["a"]).astype(bool)
    b = pd.Series(body["b"]).astype(bool)
    result = bayes_theorem(a, b)
    self.publish(result)
