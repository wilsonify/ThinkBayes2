from receive_bayes.config import (
    timeout,
    heartbeat,
    cred,
    done_exchange,
    fail_exchange,
    try_exchange,
    routing_key,
    amqp_host,
    amqp_port,
    connection_parameters
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
    odds_to_probability_strategy,
    probability_to_odds_strategy,
    oliver_blood_strategy
)
from receive_bayes.chap07 import (
    hockey_strategy,
    overtime_strategy,
    overtime2_strategy,
    shut_out_strategy,
    soccer_strategy
)
from receive_bayes.chap09 import (
    paintball_strategy,
    paintballing_strategy,
    bugs_strategy,
    gps_strategy
)
from receive_bayes.chap11 import (
    euro_hierarchical_strategy,
    euro_hierarchical_cheat_strategy,
    euro_hierarchical_uniform_strategy
)

from receive_bayes.echo import echo_strategy
from receive_bayes.mysqrt import sqrt_strategy
from receive_bayes.mystrength import strength_strategy
