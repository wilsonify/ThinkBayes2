from typing import Tuple

from rest_bayes.rpc import RemoteProcedure


def sqrt(body) -> Tuple[dict, int]:
    body['strategy'] = "sqrt"
    rpc = RemoteProcedure(routing_key='think-bayes')
    response_body, status_code = rpc.call(body)
    out_dict = response_body
    print(f"out_dict = {out_dict}")
    print(f"status_code = {status_code}")
    return out_dict


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
