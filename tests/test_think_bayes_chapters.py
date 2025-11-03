"""Tests for think-bayes chapter modules (c01-c20)."""

import pytest
import numpy as np
import pandas as pd
from collections import Counter

# Add the src directory to Python path
import sys
import os
sys.path.insert(0, '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-bayes')

try:
    import thinkbayes
    CHAPTER_MODULES_AVAILABLE = True
except ImportError:
    CHAPTER_MODULES_AVAILABLE = False
    thinkbayes = None

# Try to import chapter modules
c01_probability = c02_bayes_theorem = c03_distributions = None
c04_proportions = c05_counts = c07_mixture = None
c09_decisions = c11_comparison = c17_regression = None

if CHAPTER_MODULES_AVAILABLE:
    try:
        from thinkbayes import c01_probability, c02_bayes_theorem, c03_distributions
        from thinkbayes import c04_proportions, c05_counts, c07_mixture
        from thinkbayes import c09_decisions, c11_comparison, c17_regression
    except ImportError:
        pass


class TestChapter01:
    """Test Chapter 1: Probability functions."""
    
    @pytest.mark.skipif(c01_probability is None, reason="c01_probability module not available")
    def test_values_function(self):
        """Test values function."""
        series = pd.Series([1, 2, 2, 3, 3, 3])
        result = c01_probability.values(series)
        expected = [1, 2, 3]
        assert sorted(result) == sorted(expected)
        
    def test_prob_function(self):
        """Test prob function."""
        # Test with simple series
        series = pd.Series([1, 2, 2, 3, 3, 3])
        result = c01_probability.prob(series, 2)
        expected = 2/6  # Two 2s out of 6 total
        assert abs(result - expected) < 1e-10
        
    def test_count_function(self):
        """Test count function."""
        series = pd.Series([1, 2, 2, 3, 3, 3])
        result = c01_probability.count(series, 2)
        assert result == 2
        
    def test_conditional_function(self):
        """Test conditional probability function."""
        # Test simple conditional probability
        a = pd.Series([1, 1, 2, 2])
        b = pd.Series([1, 2, 1, 2])
        result = c01_probability.conditional(a, b, 1, 1)
        expected = 0.5  # P(A=1|B=1) = 1/2
        assert abs(result - expected) < 1e-10
        
    def test_conjunction_function(self):
        """Test conjunction probability function."""
        a = pd.Series([1, 1, 2, 2])
        b = pd.Series([1, 2, 1, 2])
        result = c01_probability.conjunction(a, b, 1, 1)
        expected = 0.25  # P(A=1 and B=1) = 1/4
        assert abs(result - expected) < 1e-10
        
    def test_bayes_theorem_function(self):
        """Test Bayes' theorem function."""
        a = pd.Series([1, 1, 2, 2])
        b = pd.Series([1, 2, 1, 2])
        result = c01_probability.bayes_theorem(a, b, 1, 1)
        # P(A=1|B=1) = P(A=1 and B=1) / P(B=1) = 0.25 / 0.5 = 0.5
        expected = 0.5
        assert abs(result - expected) < 1e-10


class TestChapter02:
    """Test Chapter 2: Bayes' Theorem classes."""
    
    def test_cookie_class(self):
        """Test Cookie class."""
        # Create cookie PMF
        cookie = c02_bayes_theorem.Cookie()
        
        # Check that it's a Pmf
        assert isinstance(cookie, thinkbayes.Pmf)
        
        # Test with vanilla cookie data
        cookie.Update('vanilla')
        
        # Bowl 1 should be more likely after vanilla cookie
        assert cookie['Bowl 1'] > cookie['Bowl 2']
        
    def test_full_monty_class(self):
        """Test FullMonty class."""
        monty = c02_bayes_theorem.FullMonty()
        
        # Check that it's a Pmf
        assert isinstance(monty, thinkbayes.Pmf)
        
        # Test with goat reveal
        monty.Update(('A', 'B'))
        
        # Should have valid probabilities
        assert abs(monty.Total() - 1.0) < 1e-10
        
    def test_monty_class(self):
        """Test Monty class."""
        monty = c02_bayes_theorem.Monty()
        
        # Check that it's a Suite
        assert isinstance(monty, thinkbayes.Suite)
        
        # Test with player choice and goat reveal
        monty.Update(('A', 'B'))
        
        # Should have valid probabilities
        assert abs(monty.Total() - 1.0) < 1e-10
        
    def test_m_and_m_class(self):
        """Test M_and_M class."""
        mandm = c02_bayes_theorem.M_and_M('bag1')
        
        # Check that it's a Suite
        assert isinstance(mandm, thinkbayes.Suite)
        
        # Test with yellow M&M data
        mandm.Update('yellow')
        
        # Should have valid probabilities
        assert abs(mandm.Total() - 1.0) < 1e-10


class TestChapter03:
    """Test Chapter 3: Distributions."""
    
    def test_mean_function(self):
        """Test Mean function for suites."""
        # Create a simple suite
        suite = thinkbayes.Pmf({1: 0.25, 2: 0.5, 3: 0.25})
        result = c03_distributions.Mean(suite)
        expected = 2.0
        assert abs(result - expected) < 1e-10
        
    def test_make_posterior_function(self):
        """Test MakePosterior function."""
        # Test with simple dataset
        dataset = [10, 15, 20, 25]
        posterior = c03_distributions.MakePosterior(30, dataset)
        
        # Should be a valid Pmf
        assert isinstance(posterior, thinkbayes.Pmf)
        assert abs(posterior.Total() - 1.0) < 1e-10
        
        # Should have reasonable mean
        mean = posterior.Mean()
        assert 10 <= mean <= 25  # Should be within data range
        
    def test_train_class(self):
        """Test Train class."""
        train = c03_distributions.Train([100, 200, 300])
        
        # Check that it's a Suite
        assert isinstance(train, thinkbayes.Suite)
        
        # Test with observed data
        train.Update(150)
        
        # Should have valid probabilities
        assert abs(train.Total() - 1.0) < 1e-10
        
        # Lower numbers should be more likely after observing 150
        assert train[100] > train[300]


class TestChapter04:
    """Test Chapter 4: Proportions."""
    
    def test_euro_class(self):
        """Test Euro class."""
        euro = c04_proportions.Euro()
        
        # Check that it's a Suite
        assert isinstance(euro, thinkbayes.Suite)
        
        # Test with coin flip data
        euro.Update('H')
        
        # Should have valid probabilities
        assert abs(euro.Total() - 1.0) < 1e-10
        
    def test_uniform_prior_function(self):
        """Test UniformPrior function."""
        prior = c04_proportions.UniformPrior()
        
        # Should be a valid Pmf
        assert isinstance(prior, thinkbayes.Pmf)
        assert abs(prior.Total() - 1.0) < 1e-10
        
        # Should have reasonable number of hypotheses
        assert len(prior) > 100
        
    def test_triangle_prior_function(self):
        """Test TrianglePrior function."""
        prior = c04_proportions.TrianglePrior()
        
        # Should be a valid Pmf
        assert isinstance(prior, thinkbayes.Pmf)
        assert abs(prior.Total() - 1.0) < 1e-10
        
        # Should have reasonable number of hypotheses
        assert len(prior) > 100
        
    def test_beta_class(self):
        """Test Beta class."""
        beta = c04_proportions.Beta()
        
        # Check initial parameters
        assert beta.alpha == 1
        assert beta.beta == 1
        
        # Test updating
        beta.Update(heads=60, tails=40)
        assert beta.alpha == 61
        assert beta.beta == 41
        
        # Test mean calculation
        mean = beta.Mean()
        assert abs(mean - (61/102)) < 1e-10


class TestChapter05:
    """Test Chapter 5: Counts and probabilities."""
    
    def test_odds_function(self):
        """Test Odds function."""
        assert c05_counts.Odds(0.5) == 1.0
        assert c05_counts.Odds(0.75) == 3.0
        assert c05_counts.Odds(0.25) == 1/3
        
    def test_probability_function(self):
        """Test Probability function."""
        assert c05_counts.Probability(1.0) == 0.5
        assert c05_counts.Probability(3.0) == 0.75
        assert c05_counts.Probability(1/3) == 0.25


class TestChapter07:
    """Test Chapter 7: Mixture distributions."""
    
    def test_hockey_class(self):
        """Test Hockey class."""
        hockey = c07_mixture.Hockey()
        
        # Check that it's a Suite
        assert isinstance(hockey, thinkbayes.Suite)
        
        # Test with goal data
        hockey.Update(4)
        
        # Should have valid probabilities
        assert abs(hockey.Total() - 1.0) < 1e-10
        
    def test_make_goal_pmf_function(self):
        """Test MakeGoalPmf function."""
        suite = thinkbayes.Pmf({1: 0.1, 2: 0.3, 3: 0.6})
        goal_pmf = c07_mixture.MakeGoalPmf(suite, high=10)
        
        # Should be a valid Pmf
        assert isinstance(goal_pmf, thinkbayes.Pmf)
        assert abs(goal_pmf.Total() - 1.0) < 1e-10
        
        # Should have reasonable range
        assert max(goal_pmf.Values()) <= 10
        
    def test_make_goal_time_pmf_function(self):
        """Test MakeGoalTimePmf function."""
        suite = thinkbayes.Pmf({2.0: 0.5, 3.0: 0.5})
        time_pmf = c07_mixture.MakeGoalTimePmf(suite)
        
        # Should be a valid Pmf
        assert isinstance(time_pmf, thinkbayes.Pmf)
        assert abs(time_pmf.Total() - 1.0) < 1e-10
        
    def test_hockey2_class(self):
        """Test Hockey2 class."""
        hockey2 = c07_mixture.Hockey2()
        
        # Check that it's a Suite
        assert isinstance(hockey2, thinkbayes.Suite)
        
        # Test with goal time data
        hockey2.Update(11)  # 11 minutes
        hockey2.Update(3.5)  # 3.5 minutes
        
        # Should have valid probabilities
        assert abs(hockey2.Total() - 1.0) < 1e-10


class TestChapter09:
    """Test Chapter 9: Decision analysis."""
    
    def test_make_angle_suite_function(self):
        """Test MakeAngleSuite function."""
        data = [10, 20, 30, 40, 50]
        suite = c09_decisions.MakeAngleSuite(data)
        
        # Should be a valid Suite
        assert isinstance(suite, thinkbayes.Suite)
        assert abs(suite.Total() - 1.0) < 1e-10
        
    def test_normal_class(self):
        """Test Normal class."""
        normal = c09_decisions.Normal([0, 1, 2])
        
        # Check that it's a Suite and Joint
        assert isinstance(normal, thinkbayes.Suite)
        assert isinstance(normal, thinkbayes.Joint)
        
        # Test with data
        normal.Update(1.5)
        
        # Should have valid probabilities
        assert abs(normal.Total() - 1.0) < 1e-10
        
    def test_paintball_class(self):
        """Test Paintball class."""
        paintball = c09_decisions.Paintball(alpha=10, beta=15)
        
        # Check that it's a Suite and Joint
        assert isinstance(paintball, thinkbayes.Suite)
        assert isinstance(paintball, thinkbayes.Joint)
        
        # Test with hit location
        paintball.Update((5, 10))
        
        # Should have valid probabilities
        assert abs(paintball.Total() - 1.0) < 1e-10
        
    def test_beetle_class(self):
        """Test Beetle class."""
        beetle = c09_decisions.Beetle([1, 2, 3, 4, 5])
        
        # Check that it's a Suite
        assert isinstance(beetle, thinkbayes.Suite)
        
        # Test with measurement
        beetle.Update(3.2)
        
        # Should have valid probabilities
        assert abs(beetle.Total() - 1.0) < 1e-10
        
    def test_species_class(self):
        """Test Species class."""
        species = c09_decisions.Species([1, 1, 2, 3, 3, 3])
        
        # Check that it's a Suite
        assert isinstance(species, thinkbayes.Suite)
        
        # Test with new species observation
        species.Update(4)
        
        # Should have valid probabilities
        assert abs(species.Total() - 1.0) < 1e-10


class TestChapter11:
    """Test Chapter 11: Hypothesis testing."""
    
    def test_euro_class(self):
        """Test Euro class for hypothesis testing."""
        euro = c11_comparison.Euro()
        
        # Check that it's a Suite
        assert isinstance(euro, thinkbayes.Suite)
        
        # Test with coin flip data
        for _ in range(140):
            euro.Update('H')
        for _ in range(110):
            euro.Update('T')
        
        # Should have valid probabilities
        assert abs(euro.Total() - 1.0) < 1e-10
        
        # Mean should be close to observed proportion
        mean = euro.Mean()
        assert abs(mean - (140/250)) < 0.1
        
    def test_suite_likelihood_function(self):
        """Test SuiteLikelihood function."""
        suite = thinkbayes.Pmf({0.4: 0.5, 0.5: 0.5})
        data = ['H', 'H', 'T']
        
        likelihood = c11_comparison.SuiteLikelihood(suite, data)
        
        # Should be a positive number
        assert likelihood > 0
        
    def test_triangle_prior_function(self):
        """Test TrianglePrior function."""
        prior = c11_comparison.TrianglePrior()
        
        # Should be a valid Pmf
        assert isinstance(prior, thinkbayes.Pmf)
        assert abs(prior.Total() - 1.0) < 1e-10
        
        # Should have reasonable number of hypotheses
        assert len(prior) > 100


class TestChapter17:
    """Test Chapter 17: Regression."""
    
    def test_regression_model_class(self):
        """Test RegressionModel class."""
        model = c17_regression.RegressionModel(
            intercept=60.0,
            offset=1990.0,
            slope=0.5,
            sigma=25.0
        )
        
        # Check attributes
        assert model.intercept == 60.0
        assert model.offset == 1990.0
        assert model.slope == 0.5
        assert model.sigma == 25.0
        
    def test_normalize_function(self):
        """Test normalize function."""
        # Create a simple joint distribution
        joint = thinkbayes.Joint()
        joint.Set((1, 2), 10)
        joint.Set((1, 3), 20)
        joint.Set((2, 2), 30)
        
        normalized = c17_regression.normalize(joint)
        
        # Should sum to 1
        assert abs(normalized.Total() - 1.0) < 1e-10
        
    def test_make_uniform_function(self):
        """Test make_uniform function."""
        qs = np.linspace(0, 1, 11)
        uniform = c17_regression.make_uniform(qs, "test")
        
        # Should be a valid Pmf
        assert isinstance(uniform, thinkbayes.Pmf)
        assert abs(uniform.Total() - 1.0) < 1e-10
        assert len(uniform) == len(qs)
        
    def test_make_joint3_function(self):
        """Test make_joint3 function."""
        pmf1 = c17_regression.make_uniform(np.linspace(0, 1, 3), "x")
        pmf2 = c17_regression.make_uniform(np.linspace(0, 1, 2), "y")
        pmf3 = c17_regression.make_uniform(np.linspace(0, 1, 2), "z")
        
        joint = c17_regression.make_joint3(pmf1, pmf2, pmf3)
        
        # Should be a valid Pmf
        assert isinstance(joint, thinkbayes.Pmf)
        assert abs(joint.Total() - 1.0) < 1e-10
        assert len(joint) == 3 * 2 * 2  # Product of lengths
        
    def test_compute_likelihood_function(self):
        """Test compute_likelihood function."""
        # Create test data
        np.random.seed(42)
        x = np.random.randn(10)
        y = 2 * x + 1 + 0.1 * np.random.randn(10)
        
        # Create a simple model
        model = c17_regression.RegressionModel(
            intercept=1.0,
            offset=0.0,
            slope=2.0,
            sigma=0.1
        )
        
        likelihood = c17_regression.compute_likelihood(x, y, model)
        
        # Should be a positive number
        assert likelihood > 0
        
    def test_update_posterior_function(self):
        """Test update_posterior function."""
        # Create simple prior
        prior = c17_regression.make_uniform(np.linspace(0, 1, 3), "test")
        
        # Create simple likelihood
        likelihood = thinkbayes.Pmf({0: 0.1, 0.5: 0.8, 1: 0.1})
        
        posterior = c17_regression.update_posterior(prior, likelihood)
        
        # Should be a valid Pmf
        assert isinstance(posterior, thinkbayes.Pmf)
        assert abs(posterior.Total() - 1.0) < 1e-10


if __name__ == "__main__":
    pytest.main([__file__])
