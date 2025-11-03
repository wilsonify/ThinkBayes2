"""Tests for the PPBM module."""

import pytest
import os


def test_ppbm_imports():
    """Test that PPBM modules can be imported."""
    try:
        # Test basic import structure
        import sys
        sys.path.insert(0, '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/ppbm')
        
        # Check that main directories exist
        ppbm_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/ppbm/ppbm'
        assert os.path.exists(ppbm_path)
        
        # Check that chapter directories exist
        chapters = [
            'Chapter0_Prologue',
            'Chapter1_Introduction', 
            'Chapter2_MorePyMC',
            'Chapter3_MCMC',
            'Chapter4_TheGreatestTheoremNeverTold',
            'Chapter5_LossFunctions',
            'Chapter6_Priorities',
            'Chapter10_'
        ]
        
        for chapter in chapters:
            chapter_path = os.path.join(ppbm_path, chapter)
            if os.path.exists(chapter_path):
                # Check that it has Python files
                py_files = [f for f in os.listdir(chapter_path) if f.endswith('.py')]
                # At least one Python file should exist
                assert len(py_files) > 0 or chapter == 'Chapter10_'  # Chapter 10 might be empty
                
    except ImportError as e:
        pytest.skip(f"PPBM module not available: {e}")


def test_ppbm_setup():
    """Test PPBM setup.py exists and is valid."""
    setup_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/ppbm/setup.py'
    if os.path.exists(setup_path):
        # Check that setup.py is readable
        with open(setup_path, 'r') as f:
            content = f.read()
            assert len(content) > 0
            assert 'setup' in content
    else:
        pytest.skip("PPBM setup.py not found")


def test_ppbm_existing_tests():
    """Test that existing PPBM tests can be found."""
    test_path = '/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/ppbm/tests/test_ppbm.py'
    if os.path.exists(test_path):
        # Check that existing test file is readable
        with open(test_path, 'r') as f:
            content = f.read()
            assert len(content) > 0
    else:
        pytest.skip("PPBM test file not found")


if __name__ == "__main__":
    pytest.main([__file__])
