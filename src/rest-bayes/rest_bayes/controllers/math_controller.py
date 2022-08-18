from typing import Tuple

from rest_bayes.rpc import RemoteProcedure


def sqrt(body) -> Tuple[dict, int]:
    body['strategy'] = "sqrt"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    print(f"out_dict = {out_dict}")
    print(f"status_code = {status_code}")
    return out_dict, status_code


def strength(body: dict) -> Tuple[dict, int]:
    body['strategy'] = "strength"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def conjunction(body):
    body['strategy'] = "conjunction"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def conditional(body):
    body['strategy'] = "conditional"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def bayes(body):
    body['strategy'] = "bayes"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def cookie(body):
    body['strategy'] = "cookie"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def monty(body):
    body['strategy'] = "monty"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def mandm(body):
    body['strategy'] = "mandm"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def euro(body):
    body['strategy'] = "euro"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def odds_to_probability(body):
    body['strategy'] = "odds_to_probability"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def probability_to_odds(body):
    body['strategy'] = "probability_to_odds"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def blood(body):
    body['strategy'] = "blood"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def hockey(body):
    body['strategy'] = "hockey"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def overtime(body):
    body['strategy'] = "overtime"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def overtime2(body):
    body['strategy'] = "overtime2"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def shut_out(body):
    body['strategy'] = "shut_out"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code


def soccer(body):
    body['strategy'] = "soccer"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    return out_dict, status_code
