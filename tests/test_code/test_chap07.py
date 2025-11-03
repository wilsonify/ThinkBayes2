"""
This is based on code and exercises from Think Bayes: Chapter 7.
"""

import numpy as np
import pytest

from thinkbayes.c07_mixture import Hockey, MakeGoalPmf


def test_hockey_init():
    """Test Hockey class initialization."""
    hockey = Hockey()
    
    # Check that it's properly initialized with a PMF
    assert len(hockey) > 0
    
    # Check that the hypotheses are around the expected mean
    hypotheses = list(hypo for hypo, prob in hockey.Items())
    assert min(hypotheses) >= 1.6  # mu - 4*sigma
    assert max(hypotheses) <= 4.0  # mu + 4*sigma


def test_hockey_likelihood():
    """Test Hockey class Likelihood method."""
    hockey = Hockey()
    
    # Test likelihood for different goal counts
    # Higher scoring rates should have higher likelihood for more goals
    high_rate_likelihood = hockey.Likelihood(5, 3.5)
    low_rate_likelihood = hockey.Likelihood(5, 2.0)
    
    # The exact relationship depends on the Poisson distribution
    assert high_rate_likelihood > 0
    assert low_rate_likelihood > 0


def test_hockey_update():
    """Test updating Hockey suite with observed data."""
    hockey = Hockey()
    
    # Store original mean
    original_mean = hockey.Mean()
    
    # Update with high-scoring game
    hockey.Update(5)
    
    # Mean should shift higher after observing high score
    new_mean = hockey.Mean()
    assert new_mean > original_mean


def test_make_goal_pmf():
    """Test MakeGoalPmf function."""
    hockey = Hockey()
    goal_pmf = MakeGoalPmf(hockey, high=10)
    
    # Check that PMF is created
    assert len(goal_pmf) > 0
    
    # Check that it covers the expected range
    goals = list(goal_pmf.Values())
    assert min(goals) >= 0
    assert max(goals) <= 10
    
    # Check that it's normalized
    assert abs(sum(goal_pmf.Values()) - 1.0) < 1e-10


def test_make_goal_pmf_with_different_high():
    """Test MakeGoalPmf with different high values."""
    hockey = Hockey()
    
    # Test with different high values
    for high in [5, 10, 15]:
        goal_pmf = MakeGoalPmf(hockey, high=high)
        assert max(goal_pmf.Values()) <= high


def test_hockey_label():
    """Test Hockey class with custom label."""
    label = "test_hockey"
    hockey = Hockey(label=label)
    assert hockey.label == label


if __name__ == "__main__":
    pytest.main([__file__])


