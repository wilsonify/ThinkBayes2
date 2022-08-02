import logging

import bayes


def test_smoke():
    logging.info("is anything on fire?")
    for member in dir(bayes):
        if member.startswith("_"):
            continue
        print(member)
