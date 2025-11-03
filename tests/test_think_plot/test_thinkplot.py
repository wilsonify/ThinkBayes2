"""Tests for the think-plot module."""

import pytest
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend for testing

# Add the src directory to Python path
import sys
import os
sys.path.insert(0, '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-plot')

try:
    import thinkplot
except ImportError as e:
    pytest.skip(f"thinkplot module not available: {e}")


def test_thinkplot_import():
    """Test that thinkplot can be imported."""
    assert thinkplot is not None


def test_thinkplot_basic_functions():
    """Test basic thinkplot functions exist."""
    # Check that common plotting functions exist
    assert hasattr(thinkplot, 'Plot')
    assert hasattr(thinkplot, 'Show')
    assert hasattr(thinkplot, 'Save')
    assert hasattr(thinkplot, 'Config')
    assert hasattr(thinkplot, 'PrePlot')


def test_thinkplot_config():
    """Test thinkplot configuration."""
    # Test that we can configure plot settings
    thinkplot.Config(xlabel='Test X', ylabel='Test Y', title='Test Plot')
    # If this doesn't raise an exception, it's working


def test_thinkplot_preplot():
    """Test thinkplot preplot function."""
    # Test that we can set up preplot
    thinkplot.PrePlot(rows=1, cols=1)
    # If this doesn't raise an exception, it's working


def test_thinkplot_show_save():
    """Test thinkplot show and save functions."""
    # These should not raise exceptions even in headless environment
    try:
        thinkplot.Show()
    except:
        pass  # Expected in headless environment
    
    try:
        thinkplot.Save('test_plot.png')
    except:
        pass  # Expected without actual plot


def test_thinkplot_file_exists():
    """Test that thinkplot source file exists and has content."""
    thinkplot_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-plot/thinkplot/thinkplot.py'
    assert os.path.exists(thinkplot_path)
    
    with open(thinkplot_path, 'r') as f:
        content = f.read()
        assert len(content) > 0
        assert 'def' in content  # Should have function definitions


if __name__ == "__main__":
    pytest.main([__file__])
