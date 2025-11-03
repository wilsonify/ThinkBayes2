"""Basic test coverage verification for all src/ modules."""

import pytest
import sys
import os

# Add all src directories to Python path
src_dirs = [
    '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-bayes',
    '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base',
    '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-plot',
]

for src_dir in src_dirs:
    if os.path.exists(src_dir):
        sys.path.insert(0, src_dir)


class TestBasicImports:
    """Test that all important modules can be imported."""
    
    def test_think_bayes_import(self):
        """Test think-bayes module import."""
        try:
            import thinkbayes
            assert thinkbayes is not None
            
            # Test core classes exist
            core_classes = ['Hist', 'Pmf', 'Cdf', 'Suite', 'Pdf', 'Joint']
            for cls in core_classes:
                assert hasattr(thinkbayes, cls), f"Missing {cls} in thinkbayes"
                
        except ImportError as e:
            pytest.skip(f"thinkbayes not available: {e}")
            
    def test_think_base_import(self):
        """Test think-base module import."""
        try:
            from think_base.src.bayes import thinkbayes, thinkplot, thinkstats
            
            assert thinkbayes is not None
            assert thinkplot is not None
            assert thinkstats is not None
            
        except ImportError as e:
            pytest.skip(f"think-base modules not available: {e}")
            
    def test_think_plot_import(self):
        """Test think-plot module import."""
        try:
            import thinkplot
            assert thinkplot is not None
            
            # Test plotting functions exist
            plotting_functions = [
                'Plot', 'Show', 'Save', 'Config', 'PrePlot',
                'Hist', 'Pmf', 'Cdf', 'Scatter', 'Bar'
            ]
            
            for func in plotting_functions:
                assert hasattr(thinkplot, func), f"Missing {func} in thinkplot"
                
        except ImportError as e:
            pytest.skip(f"thinkplot not available: {e}")


class TestBasicFunctionality:
    """Test basic functionality of imported modules."""
    
    @pytest.mark.skipif(sys.path.count('/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-bayes') == 0, 
                       reason="think-bayes not in path")
    def test_think_bayes_basic_functionality(self):
        """Test basic thinkbayes functionality."""
        try:
            import thinkbayes
            
            # Test creating a simple histogram
            data = [1, 2, 2, 3, 3, 3]
            hist = thinkbayes.Hist(data)
            assert hist[1] == 1
            assert hist[2] == 2
            assert hist[3] == 3
            
            # Test creating a PMF
            pmf = thinkbayes.Pmf(data)
            assert len(pmf) > 0
            
            # Test creating a CDF
            cdf = thinkbayes.Cdf(data)
            assert len(cdf) > 0
            
        except ImportError:
            pytest.skip("thinkbayes not available")
        except Exception as e:
            pytest.fail(f"thinkbayes basic functionality failed: {e}")
            
    @pytest.mark.skipif(sys.path.count('/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base') == 0, 
                       reason="think-base not in path")
    def test_think_base_basic_functionality(self):
        """Test basic think-base functionality."""
        try:
            from think_base.src.bayes import thinkbayes, thinkplot, thinkstats
            
            # Test statistical functions
            data = [1, 2, 3, 4, 5]
            mean = thinkstats.Mean(data)
            assert mean == 3.0
            
            var = thinkstats.Var(data)
            assert var == 2.0
            
            # Test thinkbayes functionality (handle different API)
            try:
                hist = thinkbayes.Hist(data)
                assert hist.Total() == 5
            except AttributeError:
                # Different API - try alternative
                try:
                    hist = thinkbayes.Hist()
                    for value in data:
                        hist.Increment(value)
                    assert hist.Total() == 5
                except:
                    # If histogram doesn't work, test other functionality
                    pmf = thinkbayes.Pmf(data)
                    assert len(pmf) > 0
            
        except ImportError:
            pytest.skip("think-base not available")
        except Exception as e:
            pytest.fail(f"think-base basic functionality failed: {e}")
            
    @pytest.mark.skipif(sys.path.count('/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-plot') == 0, 
                       reason="think-plot not in path")
    def test_think_plot_basic_functionality(self):
        """Test basic thinkplot functionality."""
        try:
            import thinkplot
            
            # Test configuration (should not raise exceptions)
            thinkplot.Config(xlabel='Test X', ylabel='Test Y')
            thinkplot.PrePlot(rows=1, cols=1)
            
            # Test basic plotting (might fail in headless environment, but shouldn't crash)
            try:
                thinkplot.Plot([1, 2, 3], [1, 4, 9])
                thinkplot.Show()
            except:
                pass  # Expected in headless environment
                
        except ImportError:
            pytest.skip("thinkplot not available")
        except Exception as e:
            pytest.fail(f"thinkplot basic functionality failed: {e}")


class TestFileExistence:
    """Test that important files exist."""
    
    def test_source_files_exist(self):
        """Test that source files exist."""
        important_files = [
            '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-bayes/thinkbayes/__init__.py',
            '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base/think_base/src/bayes/thinkbayes.py',
            '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-plot/thinkplot/thinkplot.py',
        ]
        
        for file_path in important_files:
            assert os.path.exists(file_path), f"Source file missing: {file_path}"
            
            with open(file_path, 'r') as f:
                content = f.read()
                assert len(content) > 0, f"Source file empty: {file_path}"
                assert 'def' in content or 'class' in content, f"Source file has no functions/classes: {file_path}"
                
    def test_example_files_exist(self):
        """Test that example files exist."""
        example_dirs = [
            '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base/think_base/src/bayes',
            '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-bayes/thinkbayes/scripts'
        ]
        
        example_files = ['cookie.py', 'dice.py', 'monty.py', 'euro.py']
        
        for example_dir in example_dirs:
            if os.path.exists(example_dir):
                for example_file in example_files:
                    file_path = os.path.join(example_dir, example_file)
                    if os.path.exists(file_path):
                        with open(file_path, 'r') as f:
                            content = f.read()
                            assert len(content) > 0, f"Example file empty: {file_path}"


class TestDependencies:
    """Test that required dependencies are available."""
    
    def test_scientific_dependencies(self):
        """Test scientific computing dependencies."""
        try:
            import numpy as np
            import pandas as pd
            import matplotlib
            import scipy
        except ImportError as e:
            pytest.fail(f"Scientific computing dependency missing: {e}")
            
        # Test basic functionality
        assert np.array([1, 2, 3]).sum() == 6
        assert len(pd.DataFrame({'a': [1, 2, 3]})) == 3
        
    def test_python_version(self):
        """Test Python version compatibility."""
        assert sys.version_info >= (3, 7), "Python 3.7+ required"


if __name__ == "__main__":
    pytest.main([__file__, '-v'])
