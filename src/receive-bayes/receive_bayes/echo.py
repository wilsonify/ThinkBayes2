import logging


def echo_strategy(self, payload):
    logging.info(f"payload = {payload}")
    payload["status_code"] = 200
    self.publish(payload)
