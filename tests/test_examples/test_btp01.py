"""
Bayes Theorem Problems
This notebook presents code and exercises from Think Bayes, second edition.
Copyright 2016 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

from __future__ import print_function, division


import warnings

warnings.filterwarnings("ignore")

import numpy as np

from thinkbayes import Hist, Pmf, Cdf, Suite, Beta
from thinkbayes import thinkplot



# ## The sock problem
#
# Yuzhong Huang
#
# There are two drawers of socks. The first drawer has 40 white socks and 10 black socks; the second drawer has 20 white socks and 30 black socks. We randomly get 2 socks from a drawer, and it turns out to be a pair(same color) but we don't know the color of these socks. What is the chance that we picked the first drawer.
#


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# ## Chess-playing twins
#
# Allen Downey
#
# Two identical twins are members of my chess club, but they never show up on the same day; in fact, they strictly alternate the days they show up.  I can't tell them apart except that one is a better player than the other:  Avery beats me 60% of the time and I beat Blake 70% of the time.  If I play one twin on Monday and win, and the other twin on Tuesday and lose, which twin did I play on which day?


# Solution goes here


# Solution goes here


# Solution goes here


# ## 1984
#
# by Katerina Zoltan
#
# The place: Airstrip One. The reason: thoughtcrime. The time: ???
#
# John's parents were taken by the Thought Police and erased from all records. John is being initiated into the Youth League and must pass a test. He is asked whether his parents are good comrades. It is not clear what John's admission officer knows:
#
# 1. He may know that John's parents have been erased and that John did not give them away.
# 2. He may know only that John's parents have been erased.
# 3. He may not know that John's parents have been erased.
#
# It is a well known fact that children who have parents that are 'good comrades' have twice the chances of passing the test. However, if the admission officer knows that their parents committed thoughtcrime (but not that they have been erased yet), a child that gave his parents away has three times the chances of getting in than a child who did not give them away.
#
# And if the admission officer knows the specifics of the arrest, a child that denies that the records are false and their parents existed has a 1/3 chance of getting in, while one who pretends that his parents never existed has a 2/3 chance. Lying to an admission officer that knows the parents have been erased will ensure that the child does not get in. Telling an admission officer that your parents do not exist when he does not know this will give you a 1/3 chance of getting in.
#
# There is a 60% chance the admission officer knows nothing, a 25% chance that he knows the parents have been erased, and a 15% chance that the officer knows all of the details. John says that he never had parents and is admitted into the Youth League. What did his admission officer know?


# Solution goes here


# Solution goes here


# ### Where Am I? - The Robot Localization Problem
#
# by Kathryn Hite
#
# Bayes's Theorem proves to be extremely useful when building mobile robots that need to know where they are within an environment at any given time.  Because of the error in motion and sensor systems, a robot's knowledge of its location in the world is based on probabilities.  Let's look at a simplified example that could feasibly be scaled up to create a working localization model.
#
# **Part A:**  We have a robot that exists within a very simple environement.  The map for this environment is a row of 6 grid cells that are colored either green or red and each labeled $x_1$, $x_2$, etc.  In real life, a larger form of this grid environment could make up what is known as an occupancy grid, or a map of the world with places that the robot can go represented as green cells and obstacles as red cells.
#
# |G|R|R|G|G|G|
# |-|-|-|-|-|-|
# |$x_1$|$x_2$|$x_3$|$x_4$|$x_5$|$x_6$|
#
# The robot has a sensor that can detect color with an 80% chance of being accurate.
#
# Given that the robot gets dropped in the environment and senses **red**, what is the probability of it being in each of the six locations?


# Solution goes here


# Solution goes here


# Solution goes here


# **Part B:** This becomes an extremely useful tool as we begin to move around the map.  Let's try to get a more accurate knowledge of where the robot falls in the world by telling it to move forward one cell.
#
# The robot moves forward one cell from its previous position and the sensor reads **green**, again with an 80% accuracy rate.  Update the probability of the robot having started in each location.


# Solution goes here


# Solution goes here


# Solution goes here


# ## Red Dice problems
#
# Suppose I have a six-sided die that is red on 2 sides and blue on 4 sides, and another die that's the other way around, red on 4 sides and blue on 2.
#
# I choose a die at random and roll it, and I tell you it came up red.  What is the probability that I rolled the second die (red on 4 sides)?


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# ## Scenario B
#
# Suppose I roll the same die again.  What is the probability I get red?


# Solution goes here


# ## Scenario A
#
# Instead of rolling the same die, suppose I choosing a die at random and roll it.  What is the probability that I get red?


# Solution goes here


# ## Scenario C
#
# Now let's run a different experiment.  Suppose I choose a die and roll it.  If the outcome is red, I report the outcome.  Otherwise I choose a die again and roll again, and repeat until I get red.
#
# What is the probability that the last die I rolled is the reddish one?


# Solution goes here


# Solution goes here


# ## Scenario D
#
# Finally, suppose I choose a die and roll it over and over until I get red, then report the outcome.  What is the probability that the die I rolled is the reddish one?


# Solution goes here


# Solution goes here


# ## The bus problem
#
# Allen Downey
#
# Two buses routes run past my house, headed for Arlington and Billerica.  In theory, the Arlington bus runs every 20 minutes and the Billerica bus every 30 minutes, but by the time they get to me, the time between buses is well-modeled by exponential distributions with means 20 and 30.
#
# Part 1: Suppose I see a bus outside my house, but I can't read the destination.  What is the probability that it is an Arlington bus?
#
# Part 2: Suppose I see a bus go by, but I don't see the destination, and 3 minutes later I see another bus.  What is the probability that the second bus is going to Arlington?


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here


# Solution goes here

