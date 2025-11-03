#!/usr/bin/env python3
"""
Comprehensive test runner for ThinkBayes2 project.
Runs all tests and provides a summary of coverage.
"""

import subprocess
import sys
import os
from pathlib import Path


def run_command(cmd, cwd=None):
    """Run a command and return success status and output."""
    try:
        result = subprocess.run(
            cmd, shell=True, capture_output=True, text=True, cwd=cwd
        )
        return result.returncode == 0, result.stdout, result.stderr
    except Exception as e:
        return False, "", str(e)


def main():
    """Main test runner."""
    print("=" * 60)
    print("ThinkBayes2 Comprehensive Test Runner")
    print("=" * 60)
    
    # Get the project root directory
    project_root = Path(__file__).parent.parent
    os.chdir(project_root)
    
    test_dirs = [
        "tests/test_code",
        "tests/test_examples", 
        "tests/test_cli",
        "tests/test_think_plot",
        "tests/test_ppbm",
        "tests/test_ppbm_pymc",
        "tests/test_receive_bayes",
        "tests/test_rest_bayes",
        "src/think-base/think_base/tests",
        "src/think-bayes/tests",
        "src/think-pymc3/tests"
    ]
    
    results = {}
    
    for test_dir in test_dirs:
        if os.path.exists(test_dir):
            print(f"\n🧪 Running tests in {test_dir}...")
            success, stdout, stderr = run_command(
                f"python -m pytest {test_dir} -v --tb=short"
            )
            
            results[test_dir] = {
                'success': success,
                'stdout': stdout,
                'stderr': stderr
            }
            
            if success:
                print(f"✅ {test_dir}: PASSED")
            else:
                print(f"❌ {test_dir}: FAILED")
                if stderr:
                    print(f"   Error: {stderr[:200]}...")
        else:
            print(f"⚠️  {test_dir}: Directory not found")
            results[test_dir] = {'success': False, 'stdout': '', 'stderr': 'Directory not found'}
    
    # Summary
    print("\n" + "=" * 60)
    print("TEST SUMMARY")
    print("=" * 60)
    
    passed = sum(1 for r in results.values() if r['success'])
    total = len(results)
    
    for test_dir, result in results.items():
        status = "✅ PASSED" if result['success'] else "❌ FAILED"
        print(f"{status:12} {test_dir}")
    
    print(f"\nOverall: {passed}/{total} test directories passed")
    
    if passed == total:
        print("🎉 All tests passed!")
        return 0
    else:
        print("⚠️  Some tests failed. Check the output above for details.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
