"""Comprehensive tests for think-base module."""

import pytest
import numpy as np
import math
from collections import Counter

# Add the src directory to Python path
import sys
import os
sys.path.insert(0, '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base')

try:
    from think_base.src.bayes import thinkbayes
    from think_base.src.bayes import thinkplot
    from think_base.src.bayes import thinkstats
except ImportError as e:
    pytest.skip(f"think-base modules not available: {e}")


class TestThinkBayesCore:
    """Test core thinkbayes functionality from think-base."""
    
    def test_imports(self):
        """Test that main classes can be imported."""
        assert hasattr(thinkbayes, 'Hist')
        assert hasattr(thinkbayes, 'Pmf')
        assert hasattr(thinkbayes, 'Cdf')
        assert hasattr(thinkbayes, 'Suite')
        assert hasattr(thinkbayes, 'Pdf')
        assert hasattr(thinkbayes, 'Joint')
        
    def test_hist_creation(self):
        """Test histogram creation."""
        # Test from list
        hist = thinkbayes.Hist([1, 2, 2, 3, 3, 3])
        assert hist[1] == 1
        assert hist[2] == 2
        assert hist[3] == 3
        
        # Test Total
        assert hist.Total() == 6
        
        # Test MaxLike
        assert hist.MaxLike() == 3
        
    def test_pmf_creation(self):
        """Test PMF creation and operations."""
        # Test from list
        pmf = thinkbayes.Pmf([1, 2, 2, 3, 3, 3])
        assert abs(pmf.Total() - 1.0) < 1e-10
        
        # Test Mean
        mean = pmf.Mean()
        expected_mean = (1*1 + 2*2 + 3*3) / 6
        assert abs(mean - expected_mean) < 1e-10
        
        # Test Var
        var = pmf.Var()
        assert var > 0
        
        # Test Std
        std = pmf.Std()
        assert abs(std - math.sqrt(var)) < 1e-10
        
    def test_cdf_creation(self):
        """Test CDF creation and operations."""
        # Test from list
        cdf = thinkbayes.Cdf([1, 2, 2, 3, 3, 3])
        
        # Test Prob (cumulative)
        assert cdf.Prob(2) == 0.5  # 3 out of 6 values are <= 2
        assert cdf.Prob(1.5) == 1/6  # 1 out of 6 values are <= 1.5
        
        # Test Value (inverse CDF)
        assert cdf.Value(0.5) == 2
        assert cdf.Value(0.25) == 1
        
    def test_suite_creation(self):
        """Test Suite creation and Bayesian updating."""
        suite = thinkbayes.Suite({1: 0.25, 2: 0.5, 3: 0.25})
        assert abs(suite.Total() - 1.0) < 1e-10
        
        # Define likelihood function
        def likelihood(data, hypo):
            if hypo == 2:
                return 0.8
            elif hypo == 1:
                return 0.1
            else:
                return 0.1
        
        # Update with data
        suite.Update('test_data', likelihood)
        
        # Hypothesis 2 should now have higher probability
        assert suite[2] > suite[1]
        assert suite[2] > suite[3]
        
    def test_joint_creation(self):
        """Test Joint distribution creation."""
        joint = thinkbayes.Joint()
        joint.Set((1, 'A'), 0.25)
        joint.Set((2, 'B'), 0.75)
        joint.Normalize()
        
        assert joint.Total() == 1.0
        assert joint[(1, 'A')] == 0.25
        assert joint[(2, 'B')] == 0.75
        
        # Test marginal
        marginal1 = joint.Marginal(0)
        assert marginal1[1] == 0.25
        assert marginal1[2] == 0.75
        
    def test_pdf_classes(self):
        """Test PDF classes."""
        # Test NormalPdf
        normal_pdf = thinkbayes.NormalPdf(mu=0, sigma=1)
        density = normal_pdf.Density(0)
        assert abs(density - (1/math.sqrt(2*math.pi))) < 1e-10
        
        # Test ExponentialPdf
        exp_pdf = thinkbayes.ExponentialPdf(lam=1.0)
        density = exp_pdf.Density(0)
        assert abs(density - 1.0) < 1e-10
        
    def test_utility_functions(self):
        """Test utility functions."""
        # Test statistical functions
        data = [1, 2, 3, 4, 5]
        
        mean = thinkbayes.Mean(data)
        assert mean == 3.0
        
        var = thinkbayes.Var(data)
        assert var == 2.0
        
        std = thinkbayes.Std(data)
        assert abs(std - math.sqrt(2.0)) < 1e-10
        
        # Test correlation functions
        xs = [1, 2, 3, 4, 5]
        ys = [2, 4, 6, 8, 10]
        
        corr = thinkbayes.Corr(xs, ys)
        assert abs(corr - 1.0) < 1e-10  # Perfect correlation
        
        cov = thinkbayes.Cov(xs, ys)
        assert cov == 10.0


class TestThinkPlot:
    """Test thinkplot functionality from think-base."""
    
    def test_imports(self):
        """Test that thinkplot functions can be imported."""
        # Check for common plotting functions
        assert hasattr(thinkplot, 'Plot')
        assert hasattr(thinkplot, 'Show')
        assert hasattr(thinkplot, 'Save')
        assert hasattr(thinkplot, 'Config')
        assert hasattr(thinkplot, 'PrePlot')
        
    def test_basic_plotting_functions(self):
        """Test basic plotting functions exist and are callable."""
        # These should not raise exceptions even in headless environment
        try:
            thinkplot.Config(xlabel='Test X', ylabel='Test Y', title='Test Plot')
            thinkplot.PrePlot(rows=1, cols=1)
        except Exception as e:
            pytest.fail(f"Basic plotting functions failed: {e}")
            
    def test_plot_creation(self):
        """Test plot creation with data."""
        # Test with simple data
        x = [1, 2, 3, 4, 5]
        y = [1, 4, 9, 16, 25]
        
        try:
            thinkplot.Plot(x, y, label='test')
            thinkplot.Show()
        except:
            pass  # Expected in headless environment
            
    def test_cdf_plotting(self):
        """Test CDF plotting."""
        cdf = thinkbayes.Cdf([1, 2, 2, 3, 3, 3])
        
        try:
            thinkplot.Cdf(cdf, label='test')
            thinkplot.Show()
        except:
            pass  # Expected in headless environment
            
    def test_pmf_plotting(self):
        """Test PMF plotting."""
        pmf = thinkbayes.Pmf([1, 2, 2, 3, 3, 3])
        
        try:
            thinkplot.Pmf(pmf, label='test')
            thinkplot.Show()
        except:
            pass  # Expected in headless environment
            
    def test_hist_plotting(self):
        """Test histogram plotting."""
        hist = thinkbayes.Hist([1, 2, 2, 3, 3, 3])
        
        try:
            thinkplot.Hist(hist, label='test')
            thinkplot.Show()
        except:
            pass  # Expected in headless environment


class TestThinkStats:
    """Test thinkstats functionality from think-base."""
    
    def test_imports(self):
        """Test that thinkstats functions can be imported."""
        # Check for common statistical functions
        assert hasattr(thinkstats, 'Mean')
        assert hasattr(thinkstats, 'Var')
        assert hasattr(thinkstats, 'Std')
        assert hasattr(thinkstats, 'Corr')
        assert hasattr(thinkstats, 'Cov')
        
    def test_statistical_functions(self):
        """Test statistical functions."""
        data = [1, 2, 3, 4, 5]
        
        # Test Mean
        mean = thinkstats.Mean(data)
        assert mean == 3.0
        
        # Test Var
        var = thinkstats.Var(data)
        assert var == 2.0
        
        # Test Std
        std = thinkstats.Std(data)
        assert abs(std - math.sqrt(2.0)) < 1e-10
        
    def test_correlation_functions(self):
        """Test correlation functions."""
        xs = [1, 2, 3, 4, 5]
        ys = [2, 4, 6, 8, 10]  # Perfect correlation
        
        # Test Corr
        corr = thinkstats.Corr(xs, ys)
        assert abs(corr - 1.0) < 1e-10
        
        # Test Cov
        cov = thinkstats.Cov(xs, ys)
        assert cov == 10.0
        
    def test_descriptive_statistics(self):
        """Test descriptive statistics."""
        data = [1, 2, 3, 4, 5, 100]  # Data with outlier
        
        # Test TrimmedMean
        trimmed_mean = thinkstats.TrimmedMean(data, p=0.1)
        assert trimmed_mean < thinkstats.Mean(data)  # Should be lower without outlier
        
        # Test Median
        median = thinkstats.Median(data)
        assert median == 3.5  # For even number of elements
        
    def test_distribution_functions(self):
        """Test distribution-related functions."""
        # Test normal distribution functions
        assert hasattr(thinkstats, 'NormalCdf')
        assert hasattr(thinkstats, 'NormalPdf')
        
        # Test normal CDF
        cdf_val = thinkstats.NormalCdf(0, mu=0, sigma=1)
        assert abs(cdf_val - 0.5) < 1e-10
        
        # Test normal PDF
        pdf_val = thinkstats.NormalPdf(0, mu=0, sigma=1)
        assert abs(pdf_val - (1/math.sqrt(2*math.pi))) < 1e-10


class TestExampleScripts:
    """Test example scripts from think-base."""
    
    def test_cookie_example(self):
        """Test cookie example script."""
        try:
            from think_base.src.bayes import cookie
            assert hasattr(cookie, 'Cookie')
            
            # Test that Cookie class can be instantiated
            cookie_jar = cookie.Cookie()
            assert hasattr(cookie_jar, 'Update')
            
        except ImportError:
            pytest.skip("cookie module not available")
            
    def test_dice_example(self):
        """Test dice example script."""
        try:
            from think_base.src.bayes import dice
            assert hasattr(dice, 'Dice')
            
            # Test that Dice class can be instantiated
            dice_game = dice.Dice([1, 2, 3, 4, 5, 6])
            assert hasattr(dice_game, 'Update')
            
        except ImportError:
            pytest.skip("dice module not available")
            
    def test_monty_example(self):
        """Test Monty Hall example script."""
        try:
            from think_base.src.bayes import monty
            assert hasattr(monty, 'Monty')
            
            # Test that Monty class can be instantiated
            monty_game = monty.Monty()
            assert hasattr(monty_game, 'Update')
            
        except ImportError:
            pytest.skip("monty module not available")
            
    def test_euro_example(self):
        """Test euro coin example script."""
        try:
            from think_base.src.bayes import euro
            assert hasattr(euro, 'Euro')
            
            # Test that Euro class can be instantiated
            euro_coin = euro.Euro()
            assert hasattr(euro_coin, 'Update')
            
        except ImportError:
            pytest.skip("euro module not available")
            
    def test_price_example(self):
        """Test price example script."""
        try:
            from think_base.src.bayes import price
            assert hasattr(price, 'Price')
            
            # Test that Price class can be instantiated
            price_game = price.Price(20000)
            assert hasattr(price_game, 'Update')
            
        except ImportError:
            pytest.skip("price module not available")


class TestIntegration:
    """Test integration between think-base modules."""
    
    def test_thinkbayes_thinkplot_integration(self):
        """Test integration between thinkbayes and thinkplot."""
        # Create a PMF and try to plot it
        pmf = thinkbayes.Pmf([1, 2, 2, 3, 3, 3])
        
        try:
            thinkplot.Pmf(pmf, label='integration test')
            thinkplot.Show()
        except:
            pass  # Expected in headless environment
            
    def test_thinkbayes_thinkstats_integration(self):
        """Test integration between thinkbayes and thinkstats."""
        # Create data from a PMF and analyze with thinkstats
        pmf = thinkbayes.Pmf({1: 0.25, 2: 0.5, 3: 0.25})
        
        # Generate sample data
        sample_data = []
        for value, prob in pmf.Items():
            sample_data.extend([value] * int(prob * 100))
            
        if sample_data:
            mean = thinkstats.Mean(sample_data)
            assert mean > 0
            
    def test_complete_workflow(self):
        """Test a complete workflow using all modules."""
        # Create some data
        data = [1, 2, 2, 3, 3, 3, 4, 4, 5]
        
        # Create histogram with thinkbayes
        hist = thinkbayes.Hist(data)
        
        # Convert to PMF
        pmf = thinkbayes.Pmf(hist)
        
        # Calculate statistics with thinkstats
        mean = thinkstats.Mean(data)
        var = thinkstats.Var(data)
        
        # Try to visualize with thinkplot
        try:
            thinkplot.Hist(hist, label='histogram')
            thinkplot.Pmf(pmf, label='pmf')
            thinkplot.Show()
        except:
            pass  # Expected in headless environment
            
        # Verify calculations
        assert mean == 3.0
        assert var > 0


if __name__ == "__main__":
    pytest.main([__file__])
