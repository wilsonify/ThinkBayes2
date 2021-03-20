# github_events.py


from json import loads

import numpy as np
from requests import get

URL = "https://api.github.com/events"
if __name__ == "__main__":
    # github allows up to 10 pages of 30 events, but we will only keep the unique ones.
    ids = np.empty(300, dtype=int)

    k = 0

    for page in range(10, 0, -1):

        r = get(URL, params={"page": page})
        data = loads(r.text)
        for event in data:
            ids[k] = (event["actor"]["id"])
            k += 1

    ids = np.unique(ids.astype(int))
