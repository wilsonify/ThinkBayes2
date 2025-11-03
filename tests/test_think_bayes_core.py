"""Comprehensive tests for think-bayes core module (__init__.py)."""

import pytest
import numpy as np
import math
from collections import Counter

# Add the src directory to Python path
import sys
import os
sys.path.insert(0, '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-bayes')

try:
    import thinkbayes
except ImportError as e:
    pytest.skip(f"thinkbayes module not available: {e}")


class TestCoreFunctions:
    """Test core utility functions in thinkbayes."""
    
    def test_random_seed(self):
        """Test RandomSeed function."""
        thinkbayes.RandomSeed(42)
        # Should not raise an exception
        
    def test_odds_probability(self):
        """Test odds and probability conversion functions."""
        # Test Odds function
        assert thinkbayes.Odds(0.5) == 1.0
        assert thinkbayes.Odds(0.75) == 3.0
        assert thinkbayes.Odds(0.25) == 1/3
        
        # Test Probability function
        assert thinkbayes.Probability(1.0) == 0.5
        assert thinkbayes.Probability(3.0) == 0.75
        assert thinkbayes.Probability(1/3) == 0.25
        
        # Test Probability2 function
        assert thinkbayes.Probability2(1, 1) == 0.5
        assert thinkbayes.Probability2(3, 1) == 0.75
        
    def test_statistical_functions(self):
        """Test statistical utility functions."""
        data = [1, 2, 3, 4, 5]
        
        # Test Mean
        assert thinkbayes.Mean(data) == 3.0
        
        # Test Var
        assert thinkbayes.Var(data) == 2.0
        
        # Test Std
        assert thinkbayes.Std(data) == math.sqrt(2.0)
        
        # Test MeanVar
        mean, var = thinkbayes.MeanVar(data)
        assert mean == 3.0
        assert var == 2.0
        
    def test_correlation_functions(self):
        """Test correlation and covariance functions."""
        xs = [1, 2, 3, 4, 5]
        ys = [2, 4, 6, 8, 10]  # Perfect correlation
        
        # Test Cov
        cov = thinkbayes.Cov(xs, ys)
        assert cov == 10.0  # Should equal variance of xs * 2
        
        # Test Corr
        corr = thinkbayes.Corr(xs, ys)
        assert abs(corr - 1.0) < 1e-10  # Perfect correlation
        
        # Test SerialCorr
        serial_corr = thinkbayes.SerialCorr(xs, lag=1)
        assert abs(serial_corr - 1.0) < 1e-10  # Perfect serial correlation


class TestHist:
    """Test Hist class."""
    
    def test_hist_creation(self):
        """Test creating histograms."""
        # Test from list
        hist = thinkbayes.Hist([1, 2, 2, 3, 3, 3])
        assert hist[1] == 1
        assert hist[2] == 2
        assert hist[3] == 3
        
        # Test from dict
        hist = thinkbayes.Hist({1: 10, 2: 20, 3: 30})
        assert hist[1] == 10
        assert hist[2] == 20
        assert hist[3] == 30
        
    def test_hist_operations(self):
        """Test histogram operations."""
        hist = thinkbayes.Hist([1, 2, 2, 3, 3, 3])
        
        # Test Total
        assert hist.Total() == 6
        
        # Test MaxLike
        assert hist.MaxLike() == 3
        
        # Test Frequencies
        freqs = list(hist.Freqs())
        assert sum(freqs) == 6
        
        # Test Values
        values = list(hist.Values())
        assert set(values) == {1, 2, 3}
        
        # Test Items
        items = list(hist.Items())
        assert len(items) == 3
        assert (1, 1) in items
        assert (2, 2) in items
        assert (3, 3) in items
        
    def test_hist_copy_and_arithmetic(self):
        """Test histogram copying and arithmetic."""
        hist1 = thinkbayes.Hist({1: 10, 2: 20})
        hist2 = thinkbayes.Hist({1: 5, 3: 15})
        
        # Test Copy
        hist_copy = hist1.Copy()
        assert hist_copy[1] == 10
        assert hist_copy[2] == 20
        
        # Test addition
        hist_sum = hist1 + hist2
        assert hist_sum[1] == 15
        assert hist_sum[2] == 20
        assert hist_sum[3] == 15
        
        # Test multiplication
        hist_mult = hist1 * 2
        assert hist_mult[1] == 20
        assert hist_mult[2] == 40


class TestPmf:
    """Test Pmf class."""
    
    def test_pmf_creation(self):
        """Test creating PMFs."""
        # Test from list
        pmf = thinkbayes.Pmf([1, 2, 2, 3, 3, 3])
        assert len(pmf) == 3
        assert pmf.Total() == 1.0  # Should be normalized
        
        # Test from dict
        pmf = thinkbayes.Pmf({1: 0.1, 2: 0.3, 3: 0.6})
        assert pmf[1] == 0.1
        assert pmf[2] == 0.3
        assert pmf[3] == 0.6
        
    def test_pmf_normalization(self):
        """Test PMF normalization."""
        pmf = thinkbayes.Pmf({1: 10, 2: 20, 3: 30})
        pmf.Normalize()
        assert abs(pmf.Total() - 1.0) < 1e-10
        
        # Test that probabilities sum to 1
        total_prob = sum(pmf.Values())
        assert abs(total_prob - 1.0) < 1e-10
        
    def test_pmf_statistics(self):
        """Test PMF statistical methods."""
        pmf = thinkbayes.Pmf({1: 0.25, 2: 0.5, 3: 0.25})
        
        # Test Mean
        mean = pmf.Mean()
        assert abs(mean - 2.0) < 1e-10
        
        # Test Var
        var = pmf.Var()
        assert abs(var - 0.5) < 1e-10
        
        # Test Std
        std = pmf.Std()
        assert abs(std - math.sqrt(0.5)) < 1e-10
        
    def test_pmf_operations(self):
        """Test PMF operations."""
        pmf = thinkbayes.Pmf({1: 0.25, 2: 0.5, 3: 0.25})
        
        # Test Prob
        assert pmf.Prob(2) == 0.5
        assert pmf.Prob(4) == 0.0
        
        # Test MaxLike
        assert pmf.MaxLike() == 2
        
        # Test Percentile
        p50 = pmf.Percentile(50)
        assert p50 == 2
        
        # Test CredibleInterval
        ci = pmf.CredibleInterval(90)
        assert len(ci) == 2
        assert ci[0] <= ci[1]


class TestCdf:
    """Test Cdf class."""
    
    def test_cdf_creation(self):
        """Test creating CDFs."""
        # Test from list
        cdf = thinkbayes.Cdf([1, 2, 2, 3, 3, 3])
        assert len(cdf) == 6
        
        # Test from items
        items = [(1, 0.1), (2, 0.4), (3, 1.0)]
        cdf = thinkbayes.Cdf(items)
        assert len(cdf) == 3
        
    def test_cdf_properties(self):
        """Test CDF properties."""
        cdf = thinkbayes.Cdf([1, 2, 2, 3, 3, 3])
        
        # Test Prob (less than)
        assert cdf.Prob(2) == 0.5  # 3 out of 6 values are <= 2
        assert cdf.Prob(1.5) == 1/6  # 1 out of 6 values are <= 1.5
        
        # Test Value (inverse CDF)
        assert cdf.Value(0.5) == 2
        assert cdf.Value(0.25) == 1
        
    def test_cdf_statistics(self):
        """Test CDF statistical methods."""
        cdf = thinkbayes.Cdf([1, 2, 2, 3, 3, 3])
        
        # Test Mean
        mean = cdf.Mean()
        assert abs(mean - 2.3333333333333335) < 1e-10
        
        # Test Var
        var = cdf.Var()
        assert abs(var - 0.5555555555555556) < 1e-10
        
        # Test Std
        std = cdf.Std()
        assert abs(std - math.sqrt(0.5555555555555556)) < 1e-10


class TestSuite:
    """Test Suite class."""
    
    def test_suite_creation(self):
        """Test creating Bayesian suites."""
        suite = thinkbayes.Suite({1: 0.25, 2: 0.5, 3: 0.25})
        assert suite.Total() == 1.0
        
    def test_suite_update(self):
        """Test Bayesian updating."""
        suite = thinkbayes.Suite({1: 0.25, 2: 0.5, 3: 0.25})
        
        # Define likelihood function
        def likelihood(data, hypo):
            if hypo == 2:
                return 0.8  # High likelihood for hypothesis 2
            elif hypo == 1:
                return 0.1
            else:
                return 0.1
        
        # Update with data
        suite.Update('test_data', likelihood)
        
        # Hypothesis 2 should now have higher probability
        assert suite[2] > suite[1]
        assert suite[2] > suite[3]
        
    def test_suite_likelihood(self):
        """Test suite likelihood computation."""
        suite = thinkbayes.Suite({1: 0.25, 2: 0.5, 3: 0.25})
        
        def likelihood(data, hypo):
            return hypo / 3.0  # Simple likelihood function
        
        # Test likelihood computation
        suite.UpdateSet(['data1', 'data2'], likelihood)
        assert abs(suite.Total() - 1.0) < 1e-10


class TestJoint:
    """Test Joint distribution class."""
    
    def test_joint_creation(self):
        """Test creating joint distributions."""
        joint = thinkbayes.Joint()
        joint.Set((1, 'A'), 0.25)
        joint.Set((2, 'B'), 0.75)
        joint.Normalize()
        
        assert joint.Total() == 1.0
        assert joint[(1, 'A')] == 0.25
        assert joint[(2, 'B')] == 0.75
        
    def test_joint_marginal(self):
        """Test computing marginal distributions."""
        joint = thinkbayes.Joint()
        joint.Set((1, 'A'), 0.25)
        joint.Set((1, 'B'), 0.25)
        joint.Set((2, 'A'), 0.25)
        joint.Set((2, 'B'), 0.25)
        joint.Normalize()
        
        # Marginal over first dimension
        marginal1 = joint.Marginal(0)
        assert marginal1[1] == 0.5
        assert marginal1[2] == 0.5
        
        # Marginal over second dimension
        marginal2 = joint.Marginal(1)
        assert marginal2['A'] == 0.5
        assert marginal2['B'] == 0.5


class TestPdf:
    """Test PDF classes."""
    
    def test_normal_pdf(self):
        """Test NormalPdf class."""
        pdf = thinkbayes.NormalPdf(mu=0, sigma=1)
        
        # Test density at mean
        density = pdf.Density(0)
        assert abs(density - (1/math.sqrt(2*math.pi))) < 1e-10
        
        # Test Laplace method
        laplace = pdf.Laplace()
        assert abs(laplace - 0.0) < 1e-10  # Should be close to mean
        
    def test_exponential_pdf(self):
        """Test ExponentialPdf class."""
        pdf = thinkbayes.ExponentialPdf(lam=1.0)
        
        # Test density at 0
        density = pdf.Density(0)
        assert abs(density - 1.0) < 1e-10
        
        # Test that density decreases with x
        assert pdf.Density(1) < pdf.Density(0)
        assert pdf.Density(2) < pdf.Density(1)


class TestBeta:
    """Test Beta distribution class."""
    
    def test_beta_creation(self):
        """Test creating Beta distributions."""
        beta = thinkbayes.Beta(alpha=2, beta=3)
        assert beta.alpha == 2
        assert beta.beta == 3
        
    def test_beta_update(self):
        """Test Beta distribution updating."""
        beta = thinkbayes.Beta(alpha=1, beta=1)  # Uniform prior
        
        # Update with evidence
        beta.Update(heads=3, tails=2)
        
        assert beta.alpha == 4  # 1 + 3 heads
        assert beta.beta == 3   # 1 + 2 tails
        
    def test_beta_mean(self):
        """Test Beta distribution mean."""
        beta = thinkbayes.Beta(alpha=2, beta=3)
        mean = beta.Mean()
        assert abs(mean - (2/5)) < 1e-10


class TestUtilityFunctions:
    """Test utility functions."""
    
    def test_make_normal_pmf(self):
        """Test MakeNormalPmf function."""
        pmf = thinkbayes.MakeNormalPmf(mu=0, sigma=1, num_sigmas=3, n=101)
        assert abs(pmf.Total() - 1.0) < 1e-10
        assert len(pmf) == 101
        
        # Test that mean is close to 0
        mean = pmf.Mean()
        assert abs(mean) < 0.1  # Should be close to 0
        
    def test_make_binomial_pmf(self):
        """Test MakeBinomialPmf function."""
        pmf = thinkbayes.MakeBinomialPmf(n=10, p=0.5)
        assert abs(pmf.Total() - 1.0) < 1e-10
        assert len(pmf) == 11  # 0 to 10 successes
        
        # Test that mean is n*p
        mean = pmf.Mean()
        assert abs(mean - 5.0) < 1e-10
        
    def test_standard_normal_cdf(self):
        """Test StandardNormalCdf function."""
        # Test at 0 should be 0.5
        assert abs(thinkbayes.StandardNormalCdf(0) - 0.5) < 1e-10
        
        # Test monotonicity
        assert thinkbayes.StandardNormalCdf(1) > thinkbayes.StandardNormalCdf(0)
        assert thinkbayes.StandardNormalCdf(-1) < thinkbayes.StandardNormalCdf(0)
        
    def test_eval_normal_cdf(self):
        """Test EvalNormalCdf function."""
        # Test standard normal
        assert abs(thinkbayes.EvalNormalCdf(0) - 0.5) < 1e-10
        
        # Test with different parameters
        assert abs(thinkbayes.EvalNormalCdf(0, mu=10, sigma=2) - 0.5) < 1e-10


if __name__ == "__main__":
    pytest.main([__file__])
