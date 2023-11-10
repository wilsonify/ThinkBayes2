import logging

from math import sqrt


def sqrt_strategy(self, body: dict):  # noqa: E501
    """ square root """
    logging.debug(f"body = {body}")
    logging.debug(f"type(body) = {type(body)}")
    x = body["x"]
    sqrt_output = dict(
        x=x,
        result=sqrt(x),
    )
    logging.debug(f"sqrt_output = {sqrt_output}")
    logging.debug(f"exchange= reply_{self.props.correlation_id}")
    self.publish(sqrt_output)
