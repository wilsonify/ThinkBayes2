"""
Think Bayes
This notebook presents example code and exercise solutions for Think Bayes.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
This is based on a notebook of example code from Think Bayes.
"""
import logging

from thinkbayes.c02_bayes_theorem import Cookie, FullMonty
from thinkbayes.scripts.mandm import MAndM


def cookie_bowl_strategy(self, body: dict):
    pmf = Cookie(["Bowl1", "Bowl2"])
    dataset = body["observations"]  # ["vanilla", "chocolate", "vanilla"]
    for data in dataset:
        pmf.Update(data)
    result = pmf.GetDict()  # { "Bowl1": 0.5294117647058824, "Bowl2": 0.4705882352941176 }
    logging.debug(f"result = {result}")
    self.publish(result)


def monty_hall_strategy(self, body: dict):
    pmf = FullMonty("ABC")
    dataset = body["observations"]  # ["B"]
    for data in dataset:
        pmf.Update(data)
    result = pmf.GetDict()
    self.publish(result)


def m_and_m_strategy(self, body: dict):
    suite = MAndM("AB")
    dataset = body["observations"]  # [("bag1", "yellow"),("bag2", "green"),("bag1", "blue")]
    for data in dataset:
        suite.Update(data)
    result = suite.GetDict()
    self.publish(result)
