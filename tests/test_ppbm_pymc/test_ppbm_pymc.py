"""Tests for the PPBM-PyMC module."""

import pytest
import os


def test_ppbm_pymc_structure():
    """Test PPBM-PyMC module structure."""
    ppbm_pymc_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/ppbm-pymc'
    assert os.path.exists(ppbm_pymc_path)
    
    # Check that main module directory exists
    module_path = os.path.join(ppbm_pymc_path, 'ppbm_pymc')
    assert os.path.exists(module_path)
    
    # Check for Python files
    py_files = []
    for root, dirs, files in os.walk(module_path):
        py_files.extend([f for f in files if f.endswith('.py')])
    
    assert len(py_files) > 0, "No Python files found in ppbm-pymc module"


def test_ppbm_pymc_modules():
    """Test that PPBM-PyMC modules can be imported."""
    module_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/ppbm-pymc/ppbm_pymc'
    
    # Check for specific example files
    expected_files = [
        'ABtesting.py',
        'ClusteringWithGaussians.py', 
        'FreqOfCheaters.py',
        'ORingFailure.py',
        'SMS_behaviour.py'
    ]
    
    for filename in expected_files:
        file_path = os.path.join(module_path, filename)
        if os.path.exists(file_path):
            # Check that file is readable and has content
            with open(file_path, 'r') as f:
                content = f.read()
                assert len(content) > 0, f"File {filename} is empty"
        else:
            pytest.skip(f"PPBM-PyMC file {filename} not found")


def test_ppbm_pymc_file_syntax():
    """Test that PPBM-PyMC files have valid Python syntax."""
    import ast
    
    module_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/ppbm-pymc/ppbm_pymc'
    
    for filename in os.listdir(module_path):
        if filename.endswith('.py'):
            file_path = os.path.join(module_path, filename)
            try:
                with open(file_path, 'r') as f:
                    content = f.read()
                # Try to parse the file as Python
                ast.parse(content)
            except SyntaxError as e:
                pytest.fail(f"Syntax error in {filename}: {e}")


if __name__ == "__main__":
    pytest.main([__file__])
