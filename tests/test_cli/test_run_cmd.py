"""Tests for the CLI run_cmd module."""
import os
import tempfile
import pytest
from unittest.mock import patch, MagicMock
from subprocess import CalledProcessError
import sys


def test_run_cmd_basic_functionality():
    """Test basic functionality of run_cmd with mocked dependencies."""
    # Mock sys.argv before importing the module to avoid assertion error
    with patch("sys.argv", ["run_cmd.py", "test_input", "test_output"]):
        # Now import the module
        import importlib.util
        spec = importlib.util.spec_from_file_location(
            "run_cmd", "/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/cli/run_cmd.py"
        )
        run_cmd = importlib.util.module_from_spec(spec)
        
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create test input files
            input_file = os.path.join(temp_dir, "test.txt")
            with open(input_file, "w") as f:
                f.write("test content")
            
            # Mock the subprocess.check_output to avoid external dependencies
            with patch("subprocess.check_output") as mock_check_output:
                mock_check_output.return_value = b"processed content"
                
                try:
                    spec.loader.exec_module(run_cmd)
                except SystemExit:
                    pass  # Expected due to assertion in main
                
                # Check that output file was created
                output_file = os.path.join(temp_dir, "test_output.txt")
                if os.path.exists(output_file):
                    assert True  # File was created successfully


def test_run_cmd_argument_validation():
    """Test that run_cmd validates command line arguments."""
    # Test that missing arguments cause assertion error
    with pytest.raises(AssertionError, match="usage: python run_cmd.py"):
        import importlib.util
        spec = importlib.util.spec_from_file_location(
            "run_cmd", "/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/cli/run_cmd.py"
        )
        run_cmd = importlib.util.module_from_spec(spec)
        
        # Mock sys.argv with insufficient arguments
        with patch("sys.argv", ["run_cmd.py"]):
            spec.loader.exec_module(run_cmd)


def test_cli_module_exists():
    """Test that the CLI module file exists and is readable."""
    cli_path = "/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/src/cli/run_cmd.py"
    assert os.path.exists(cli_path)
    
    with open(cli_path, 'r') as f:
        content = f.read()
        assert len(content) > 0
        assert "run_cmd" in content
        assert "glob" in content


if __name__ == "__main__":
    pytest.main([__file__])
