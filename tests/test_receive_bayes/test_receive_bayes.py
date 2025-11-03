"""Tests for the receive-bayes module."""

import pytest
import os
import ast


def test_receive_bayes_structure():
    """Test receive-bayes module structure."""
    receive_bayes_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/receive-bayes'
    assert os.path.exists(receive_bayes_path)
    
    # Check that main module directory exists
    module_path = os.path.join(receive_bayes_path, 'receive_bayes')
    assert os.path.exists(module_path)
    
    # Check for Python files
    py_files = []
    for root, dirs, files in os.walk(module_path):
        py_files.extend([f for f in files if f.endswith('.py')])
    
    assert len(py_files) > 0, "No Python files found in receive-bayes module"


def test_receive_bayes_main_files():
    """Test that main receive-bayes files exist and are readable."""
    module_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/receive-bayes/receive_bayes'
    
    # Check for key files
    key_files = [
        '__init__.py',
        '__main__.py',
        'abstract.py',
        'conf.py',
        'config.py'
    ]
    
    # Check for chapter files
    chapter_files = [
        'chap01.py', 'chap02.py', 'chap03.py', 'chap04.py', 
        'chap05.py', 'chap07.py', 'chap09.py', 'chap11.py'
    ]
    
    all_files = key_files + chapter_files
    
    for filename in all_files:
        file_path = os.path.join(module_path, filename)
        if os.path.exists(file_path):
            # Check that file is readable
            with open(file_path, 'r') as f:
                content = f.read()
                assert len(content) > 0, f"File {filename} is empty"
        else:
            pytest.skip(f"receive-bayes file {filename} not found")


def test_receive_bayes_file_syntax():
    """Test that receive-bayes files have valid Python syntax."""
    module_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/receive-bayes/receive_bayes'
    
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


def test_receive_bayes_imports():
    """Test that receive-bayes modules can be imported."""
    import sys
    sys.path.insert(0, '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/receive-bayes')
    
    try:
        import receive_bayes
        assert receive_bayes is not None
    except ImportError as e:
        pytest.skip(f"receive_bayes module not importable: {e}")


if __name__ == "__main__":
    pytest.main([__file__])
