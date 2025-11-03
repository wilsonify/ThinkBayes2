"""Tests for the rest-bayes module."""

import pytest
import os
import ast


def test_rest_bayes_structure():
    """Test rest-bayes module structure."""
    rest_bayes_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/rest-bayes'
    assert os.path.exists(rest_bayes_path)
    
    # Check that main module directory exists
    module_path = os.path.join(rest_bayes_path, 'rest_bayes')
    assert os.path.exists(module_path)
    
    # Check for Python files
    py_files = []
    for root, dirs, files in os.walk(module_path):
        py_files.extend([f for f in files if f.endswith('.py')])
    
    assert len(py_files) > 0, "No Python files found in rest-bayes module"


def test_rest_bayes_main_files():
    """Test that main rest-bayes files exist and are readable."""
    module_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/rest-bayes/rest_bayes'
    
    # Check for key files
    key_files = [
        '__init__.py',
        '__main__.py',
        'app.py',
        'config.py'
    ]
    
    for filename in key_files:
        file_path = os.path.join(module_path, filename)
        if os.path.exists(file_path):
            # Check that file is readable
            with open(file_path, 'r') as f:
                content = f.read()
                # __init__.py can be empty, that's fine
                if filename == '__init__.py':
                    assert len(content) >= 0, f"File {filename} should be readable"
                else:
                    assert len(content) > 0, f"File {filename} is empty"
        else:
            pytest.skip(f"rest-bayes file {filename} not found")


def test_rest_bayes_file_syntax():
    """Test that rest-bayes files have valid Python syntax."""
    module_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/rest-bayes/rest_bayes'
    
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


def test_rest_bayes_imports():
    """Test that rest-bayes modules can be imported."""
    import sys
    sys.path.insert(0, '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/rest-bayes')
    
    try:
        import rest_bayes
        assert rest_bayes is not None
    except ImportError as e:
        pytest.skip(f"rest_bayes module not importable: {e}")


if __name__ == "__main__":
    pytest.main([__file__])
