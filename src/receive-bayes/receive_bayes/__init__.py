import logging
import os

import pika

logging.getLogger(__name__).addHandler(logging.NullHandler())
amqp_host = os.getenv("AMQP_HOST", "localhost")
amqp_port = os.getenv("AMQP_PORT", "5672")
routing_key = os.getenv("AMQP_ROUTING_KEY", "think-bayes")
heartbeat = os.getenv("AMQP_HEARTBEAT", "10000")
timeout = os.getenv("AMQP_TIMEOUT", "10001")
cred = pika.PlainCredentials(
    os.getenv("AMQP_USER", "guest"),
    os.getenv("AMQP_PASS", "guest")
)
try_exchange = f"try_{routing_key}"
done_exchange = f"done_{routing_key}"
fail_exchange = f"fail_{routing_key}"
connection_parameters = pika.ConnectionParameters(
    host=amqp_host,
    port=int(amqp_port),
    heartbeat=int(heartbeat),
    blocked_connection_timeout=int(timeout),
    credentials=cred,
)

from receive_bayes.abstract import Strategy
from receive_bayes.chap02 import (
    cookie_bowl_strategy,
    monty_hall_strategy,
    m_and_m_strategy
)

from receive_bayes.chap03 import (
    dice_strategy,
    train_strategy
)
from receive_bayes.chap04 import (
    euro_strategy
)
from receive_bayes.chap05 import (
    Probability_strategy,
    odds_strategy,
    Oliver_strategy
)
from receive_bayes.chap09 import (
    paintball_strategy,
    paintballing_strategy,
    bugs_strategy,
    gps_strategy
)
from receive_bayes.chap11 import euro_strategy
from receive_bayes.echo import echo_strategy
from receive_bayes.mysqrt import sqrt_strategy
from receive_bayes.mystrength import strength_strategy
