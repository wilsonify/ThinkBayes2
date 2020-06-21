#!/usr/bin/python

"""This file contains code for use with "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2013 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import csv
import json
import logging
import os
import sys
from datetime import datetime, time
from time import sleep
from urllib.request import urlopen

import numpy
import redis as redis


class Redis(object):
    """Provides access to a Redis instance on Redis To Go"""

    host = "dory.redistogo.com"
    port = 10534

    def __init__(self):
        try:
            password = os.environ["REDIS_AUTH"]
        except KeyError:
            print("Environment variable REDIS_AUTH is not set.")
            sys.exit()

        self.r = redis.StrictRedis(
            host=self.host, port=self.port, password=password, db=0
        )

    def write_train_spotting(self, timestamp, tripid, seconds, live=True):
        """Writes a trainspotting event to the database.

        timestamp: int seconds since epoch
        tripid: string unique id
        seconds: int how many seconds away the train is
        live: boolean, whether to actually write the data
        """
        dt = datetime.fromtimestamp(timestamp)
        day = dt.date().isoformat()

        print(dt, tripid, seconds, timestamp)

        if live:
            self.r.sadd("days", day)
            self.r.sadd(day, tripid)
            self.r.zadd(tripid, seconds, timestamp)

    def find_arrivals(self, start_hour=16, end_hour=18):
        """For each trip, find the best estimate of the arrival time.

        start_hour: int 0-24, beginning of observation window
        end_hour: int 0-24, end of window

        Returns: map from string day to unsorted list of arrival datetimes
        """
        days = self.r.smembers("days")
        print(days)

        start_time = time(hour=start_hour)
        end_time = time(hour=end_hour)

        arrival_map = {}

        for day in days:
            tripids = self.r.smembers(day)

            for tripid in tripids:
                pred_dt = self.get_predicted_arrival(tripid)
                pred_time = pred_dt.time()

                if start_time < pred_time < end_time:
                    arrival_map.setdefault(day, []).append(pred_dt)

        return arrival_map

    def get_predicted_arrival(self, tripid):
        """Gets the best predicted arrival time for a given trip.

        tripid: string TripID like R98313D88
        """
        pair = self.r.zrange(tripid, 0, 1, withscores=True)
        timestamp, seconds = pair[0]
        pred_ts = float(timestamp) + seconds
        pred_dt = datetime.fromtimestamp(pred_ts)
        return pred_dt


class TrainSpotting(object):
    """Represents one observation of a train."""

    def __init__(self, t):
        self.timestamp = int(t[0])
        self.tripid = t[2]
        self.seconds = int(t[6])


def read_csv(url="https://developer.mbta.com/lib/rthr/red.csv"):
    """Reads data from the red line.

    Returns: list of TrainSpotting objects
    """
    fp = urlopen(url)
    reader = csv.reader(fp)

    tss = []
    for t in reader:
        if t[5] != "Kendall/MIT":
            continue
        if t[3] != "Braintree":
            continue

        ts = TrainSpotting(t)
        tss.append(ts)

    fp.close()
    return tss


def read_json():
    url = "https://developer.mbta.com/lib/rthr/red.json"
    json_text = urlopen(url).read()
    json_obj = json.loads(json_text)
    print(json_obj)


def read_and_store(red):
    """Read data from the MBTA and put it in the database.

    red: Redis object
    """
    tss = read_csv()
    for ts in tss:
        red.write_train_spotting(ts.timestamp, ts.tripid, ts.seconds)


def loop(red, start_time, end_time, delay=60):
    """Collects data from start_time until end_time.

    red: Redis object to store data
    start_time: datetime
    end_time: datetime
    delay: time to sleep between collections, in seconds
    """
    if datetime.now() < start_time:
        diff = start_time - datetime.now()
        print("Sleeping", diff)
        sleep(diff.total_seconds())

    while datetime.now() < end_time:
        print("Collecting")
        read_and_store(red)
        sleep(delay)


def today_at(hour):
    """Makes a datetime object with today's date and the given time.

    hour: int 0-24
    """
    now = datetime.now()
    return datetime.combine(now, time(hour=hour))


def get_interarrivals(arrival_map):
    """Finds all interarrival times in the arrival map.

    arrival_map: map from string day to unsorted list of arrival datetimes

    Returns: list of float interarrival times in seconds
    """
    interarrival_seconds = []
    for day, arrivals in sorted(arrival_map.items()):
        print(day, len(arrivals))
        arrivals.sort()
        diffs = numpy.diff(arrivals)
        diffs = [diff.total_seconds() for diff in diffs]
        interarrival_seconds.extend(diffs)

    return interarrival_seconds


def main(script, command="collect"):
    logging.debug("%r", f"script={script}")
    red = Redis()

    if command == "collect":
        start = today_at(16)
        end = today_at(18)

        print(start, end)
        loop(red, start, end)

    elif command == "report":
        arrival_map = red.find_arrivals()
        interarrivals = get_interarrivals(arrival_map)
        print(repr(interarrivals))


if __name__ == "__main__":
    main(*sys.argv)
