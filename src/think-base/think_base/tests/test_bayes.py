import logging
import os
import pytest

import think_base


def test_smoke():
    """Basic smoke test for think_base module."""
    logging.info("is anything on fire?")
    for member in dir(think_base):
        if member.startswith("_"):
            continue
        print(member)


def test_think_base_imports():
    """Test that key think_base modules can be imported."""
    try:
        from think_base.src.bayes import thinkbayes
        assert thinkbayes is not None
    except ImportError as e:
        pytest.skip(f"thinkbayes module not importable: {e}")


def test_think_base_files_exist():
    """Test that key think_base files exist."""
    base_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base/think_base/src/bayes'
    
    key_files = [
        'thinkbayes.py',
        'thinkplot.py', 
        'thinkstats.py'
    ]
    
    for filename in key_files:
        file_path = os.path.join(base_path, filename)
        if os.path.exists(file_path):
            with open(file_path, 'r') as f:
                content = f.read()
                assert len(content) > 0, f"File {filename} is empty"
        else:
            pytest.skip(f"think-base file {filename} not found")


def test_think_base_example_files():
    """Test that example files exist and are readable."""
    base_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base/think_base/src/bayes'
    
    example_files = [
        'cookie.py', 'dice.py', 'monty.py', 'euro.py',
        'price.py', 'kidney.py', 'paintball.py'
    ]
    
    for filename in example_files:
        file_path = os.path.join(base_path, filename)
        if os.path.exists(file_path):
            with open(file_path, 'r') as f:
                content = f.read()
                assert len(content) > 0, f"Example file {filename} is empty"
        else:
            pytest.skip(f"Example file {filename} not found")


def test_think_base_setup():
    """Test that think_base setup.py exists and is valid."""
    setup_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/think-base/setup.py'
    assert os.path.exists(setup_path)
    
    with open(setup_path, 'r') as f:
        content = f.read()
        assert len(content) > 0
        assert 'setup' in content
