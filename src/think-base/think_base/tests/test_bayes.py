import logging

import think_base


def test_smoke():
    logging.info("is anything on fire?")
    for member in dir(think_base):
        if member.startswith("_"):
            continue
        print(member)
