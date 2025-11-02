# Developer Guide

## Introduction

This developer guide provides a comprehensive framework for contributing to the Think Bayes 2 codebase. The project represents a unique intersection of statistical computation, educational methodology, and software engineering practices. Understanding the architectural decisions and development patterns will enable you to make meaningful contributions while maintaining the project's educational and scientific integrity.

The development approach prioritizes clarity, reproducibility, and pedagogical value. Every code change should serve the dual purpose of functional correctness and educational accessibility. This guide will walk you through the technical infrastructure, development workflow, and contribution philosophy that underpins the project.

## Development Environment Configuration

### System Architecture Requirements

The Think Bayes 2 project operates within a specific computational ecosystem that has been optimized for both educational use and research applications. Understanding these requirements ensures consistency across development environments and facilitates collaboration.

#### Core Dependencies and Their Rationale

The project's dependency structure reflects careful consideration of trade-offs between performance, accessibility, and educational value:

```bash
# Essential computational frameworks
numpy>=1.21.0          # Fundamental numerical computing
pandas>=1.3.0          # Data manipulation and analysis
scipy>=1.7.0           # Scientific computing and statistical functions
matplotlib>=3.5.0      # Visualization foundation
sympy>=1.9             # Symbolic mathematics for theoretical work

# Development and testing infrastructure
pytest>=7.0.0          # Testing framework with extensive plugin ecosystem
nbmake>=1.3.0          # Notebook testing for educational content
```

### Environment Setup Protocol

Establishing a consistent development environment prevents common integration issues and ensures reproducible behavior across different systems.

#### Step 1: Repository Acquisition and Initial Configuration

```bash
# Clone the repository with full history
git clone https://github.com/AllenDowney/ThinkBayes2.git
cd ThinkBayes2

# Examine the project structure
find . -name "*.py" -type f | head -20
ls -la requirements*.txt
```

#### Step 2: Virtual Environment Establishment

```bash
# Create isolated development environment
python -m venv venv-dev

# Platform-specific activation
# Windows:
venv-dev\Scripts\activate
# Unix-like systems:
source venv-dev/bin/activate

# Verify Python version and path
python --version
which python
```

#### Step 3: Development Dependencies Installation

```bash
# Install core computational libraries
pip install -r requirements.txt

# Install development-specific dependencies
pip install -r requirements-dev.txt

# Install project packages in development mode
pip install -e src/think-bayes
pip install -e src/think-plot

# Validate installation integrity
python -c "
import thinkbayes as tb
import thinkplot as tp
print('Core packages imported successfully')
print(f'Think Bayes version: {tb.__version__}')
"
```

### Development Tools Configuration

#### Integrated Development Environment Setup

While the project can be developed with any text editor, certain configurations enhance productivity and code quality:

```json
// VS Code settings for Think Bayes 2 development
{
    "python.defaultInterpreterPath": "./venv-dev/bin/python",
    "python.linting.enabled": true,
    "python.linting.pylintEnabled": true,
    "python.formatting.provider": "black",
    "python.testing.pytestEnabled": true,
    "python.testing.pytestArgs": ["tests"],
    "jupyter.jupyterServerType": "local"
}
```

#### Pre-commit Configuration

```bash
# Install pre-commit hooks for code quality assurance
pip install pre-commit
pre-commit install

# Configuration in .pre-commit-config.yaml
repos:
  - repo: https://github.com/psf/black
    rev: 22.3.0
    hooks:
      - id: black
        language_version: python3
  - repo: https://github.com/pycqa/pylint
    rev: v2.13.9
    hooks:
      - id: pylint
```

## Project Architecture and Code Organization

### Structural Design Principles

The Think Bayes 2 codebase follows several architectural principles that serve both technical and educational objectives:

1. **Modular Design**: Each component serves a specific, well-defined purpose
2. **Progressive Complexity**: Simple concepts are introduced before complex ones
3. **Consistent Interfaces**: Similar operations use similar method signatures
4. **Documentation Integration**: Code examples serve as both documentation and tests

### Directory Structure Analysis

```
ThinkBayes2/
├── src/                           # Source code packages
│   ├── think-bayes/               # Core Bayesian computation library
│   │   ├── __init__.py           # Public API definitions
│   │   ├── pmf.py                # Probability mass functions
│   │   ├── cdf.py                # Cumulative distribution functions
│   │   ├── suite.py              # Bayesian inference framework
│   │   ├── beta.py               # Beta distribution implementations
│   │   └── [chapter modules]     # Chapter-specific implementations
│   ├── think-plot/               # Visualization utilities
│   │   ├── __init__.py           # Plotting API
│   │   ├── thinkplot.py          # Core plotting functions
│   │   └── utils.py              # Visualization helpers
│   └── cli/                      # Command-line interface tools
├── tests/                        # Comprehensive test suite
│   ├── test_code/               # Unit tests for core functionality
│   ├── test_examples/           # Integration tests for examples
│   └── data/                    # Test data fixtures
├── notebooks/                   # Educational content
├── examples/                    # Standalone code examples
└── docs/                       # Documentation source
```

### Core Package Architecture

#### think-bayes Package Design

The core package implements fundamental Bayesian statistical objects with careful attention to computational efficiency and educational clarity:

```python
# Core class hierarchy design
class Pmf:
    """Probability Mass Function with Bayesian updating capabilities"""
    
    def __init__(self, values=None, name=''):
        """Initialize with optional values and descriptive name"""
        self.d = {}  # Internal dictionary representation
        self.name = name
        
    def Set(self, value, prob):
        """Set probability for specific value"""
        self.d[value] = prob
        
    def Mult(self, value, factor):
        """Multiply probability by factor (for Bayesian updating)"""
        self.d[value] = self.d.get(value, 0) * factor
        
    def Normalize(self, fraction=1.0):
        """Normalize probabilities to sum to specified fraction"""
        total = sum(self.d.values())
        if total == 0:
            return total
        
        factor = float(fraction) / total
        for value in self.d:
            self.d[value] *= factor
            
        return total
```

#### think-plot Package Architecture

The visualization package provides statistical plotting functions specifically designed for Bayesian analysis:

```python
# Statistical plotting with educational focus
def Pmf(pmf, **options):
    """Plot probability mass function with appropriate formatting"""
    
    # Extract values and probabilities
    values, probs = zip(*sorted(pmf.Items()))
    
    # Create bar plot with educational styling
    plt.bar(values, probs, **options)
    
    # Apply statistical plot formatting
    plt.xlabel('Value')
    plt.ylabel('Probability')
    plt.title(f'Probability Mass Function: {pmf.name}')
    
    return plt.gca()
```

## Development Workflow and Quality Assurance

### Testing Strategy and Implementation

The project employs a multi-layered testing approach that ensures both functional correctness and educational effectiveness.

#### Unit Testing Framework

Unit tests focus on individual components and mathematical correctness:

```python
# Example: Testing PMF mathematical properties
def test_pmf_normalization():
    """Verify that PMF normalization maintains mathematical properties"""
    
    pmf = tb.Pmf()
    pmf.Set('A', 0.3)
    pmf.Set('B', 0.7)
    
    # Test normalization
    total = pmf.Normalize()
    assert abs(total - 1.0) < 1e-10, "PMF should normalize to 1.0"
    
    # Test probability sum
    prob_sum = sum(pmf.Values())
    assert abs(prob_sum - 1.0) < 1e-10, "Probabilities should sum to 1.0"

def test_bayesian_updating():
    """Verify Bayesian updating produces mathematically correct results"""
    
    # Create prior (uniform distribution)
    prior = tb.Pmf()
    for i in range(1, 7):
        prior.Set(i, 1/6)
    
    # Apply evidence (rolled a 6)
    likelihood_6 = 1/6
    prior.Mult(6, likelihood_6)
    prior.Normalize()
    
    # Verify posterior properties
    posterior_prob_6 = prior.Prob(6)
    expected_prob_6 = (1/6 * 1/6) / ((5 * 1/6 * 1/6) + (1 * 1/6 * 1/6))
    
    assert abs(posterior_prob_6 - expected_prob_6) < 1e-10
```

#### Integration Testing for Examples

Integration tests verify that complete examples work as expected:

```python
def test_dice_example_integration():
    """Test complete dice problem example"""
    
    class Dice(tb.Suite):
        def Likelihood(self, data, hypo):
            if hypo == 4:
                return 1/4 if data <= 4 else 0
            elif hypo == 6:
                return 1/6 if data <= 6 else 0
            # ... other dice types
    
    dice = Dice([4, 6, 8, 12, 20])
    dice.Update(6)
    
    # Verify posterior properties
    assert dice.Prob(4) == 0, "4-sided die cannot roll 6"
    assert dice.Prob(6) > dice.Prob(8), "6-sided die more likely after rolling 6"
```

#### Notebook Testing

Educational content is tested to ensure examples work correctly:

```python
# Test notebook execution
def test_notebook_execution():
    """Verify that notebooks execute without errors"""
    
    import nbformat
    from nbconvert.preprocessors import ExecutePreprocessor
    
    with open('notebooks/chap01.ipynb', 'r') as f:
        nb = nbformat.read(f, as_version=4)
    
    ep = ExecutePreprocessor(timeout=600, kernel_name='python3')
    ep.preprocess(nb, {'metadata': {'path': 'notebooks/'}})
```

### Code Quality Standards

#### Style Guidelines and Rationale

The project follows specific style guidelines that prioritize readability and educational value:

```python
# Function documentation with educational examples
def MakeCdfFromPmf(pmf, name=None):
    """
    Create a cumulative distribution function from a probability mass function.
    
    This transformation is useful for:
    - Calculating percentiles and quantiles
    - Comparing distributions visually
    - Performing probability interval calculations
    
    Parameters:
    -----------
    pmf : Pmf
        Probability mass function to convert
    name : string, optional
        Name for the resulting CDF
        
    Returns:
    --------
    Cdf
        Cumulative distribution function
        
    Example:
    --------
    >>> pmf = tb.Pmf()
    >>> pmf.Set(1, 0.3); pmf.Set(2, 0.7)
    >>> cdf = tb.MakeCdfFromPmf(pmf)
    >>> cdf.Percentile(50)  # Median
    2
    """
    
    if name is None:
        name = pmf.name
        
    return MakeCdfFromDict(pmf.GetDict(), name=name)
```

### Contribution Workflow

#### Branch Strategy and Version Control

The project uses a structured branching strategy that facilitates collaboration while maintaining stability:

```bash
# Development branch creation
git checkout -b feature/beta-distribution-optimization
git checkout -b fix/normalization-precision-issue
git checkout -b docs/hierarchical-modeling-guide

# Commit message conventions
git commit -m "feat: Add conjugate prior support for binomial likelihood"
git commit -m "fix: Resolve numerical precision issues in PMF normalization"
git commit -m "docs: Update developer guide with testing procedures"
```

#### Pull Request Process

Pull requests undergo systematic review to ensure quality and consistency:

1. **Automated Checks**: Code style, test coverage, and documentation
2. **Mathematical Review**: Verify statistical correctness
3. **Educational Review**: Ensure clarity and pedagogical value
4. **Integration Testing**: Verify compatibility with existing code

```python
# Example contribution: New distribution implementation
class Gamma(tb.Suite):
    """
    Gamma distribution implementation for continuous positive variables.
    
    Useful for modeling:
    - Waiting times between events
    - Rate parameters in hierarchical models
    - Prior distributions for scale parameters
    """
    
    def __init__(self, alpha, beta):
        """
        Initialize gamma distribution with shape alpha and rate beta.
        
        Parameters:
        -----------
        alpha : float
            Shape parameter (must be positive)
        beta : float
            Rate parameter (must be positive)
        """
        self.alpha = alpha
        self.beta = beta
        
    def Likelihood(self, data, hypo):
        """Calculate likelihood for observed data"""
        # Implementation details...
        pass
```

## Advanced Development Topics

### Performance Optimization Strategies

#### Computational Efficiency Considerations

The project balances educational clarity with computational efficiency:

```python
# Efficient PMF operations for large datasets
class OptimizedPmf(tb.Pmf):
    """Performance-optimized PMF for large-scale applications"""
    
    def __init__(self, values=None, name=''):
        super().__init__(values, name)
        self._cache = {}  # Cache computed statistics
        
    def Mean(self):
        """Cached mean calculation for repeated access"""
        if 'mean' not in self._cache:
            self._cache['mean'] = super().Mean()
        return self._cache['mean']
    
    def Var(self):
        """Cached variance calculation"""
        if 'variance' not in self._cache:
            mean = self.Mean()
            var = sum(p * (x - mean)**2 for x, p in self.Items())
            self._cache['variance'] = var
        return self._cache['variance']
```

### Extension and Customization Framework

#### Plugin Architecture for Custom Distributions

The project supports extensions through well-defined interfaces:

```python
# Custom distribution interface
class CustomDistribution(tb.Suite):
    """
    Template for creating custom probability distributions.
    
    Subclasses must implement:
    - Likelihood(self, data, hypo): Likelihood function
    - Optional: Update(self, data): Custom updating logic
    """
    
    def __init__(self, hypotheses, name=''):
        super().__init__(hypotheses, name)
        
    def Likelihood(self, data, hypo):
        """Must be implemented by subclasses"""
        raise NotImplementedError("Subclasses must implement Likelihood")
        
    def PosteriorPredictiveSample(self):
        """Generate sample from posterior predictive distribution"""
        # Default implementation can be overridden
        return self.Random()
```

## Troubleshooting and Common Development Challenges

### Debugging Methodologies

#### Systematic Debugging Approach

When encountering issues, follow this structured debugging process:

1. **Isolation**: Reproduce the issue with minimal code
2. **Verification**: Check mathematical expectations
3. **Profiling**: Identify performance bottlenecks
4. **Documentation**: Record findings and solutions

```python
# Debugging utility functions
def debug_pmf_operations(pmf, operation_name="unknown"):
    """Diagnostic tool for PMF operations"""
    
    print(f"Debugging PMF operation: {operation_name}")
    print(f"PMF name: {pmf.name}")
    print(f"Number of elements: {len(pmf)}")
    print(f"Total probability: {sum(pmf.Values())}")
    print(f"Mean: {pmf.Mean()}")
    print(f"Variance: {pmf.Var()}")
    
    # Check for common issues
    if abs(sum(pmf.Values()) - 1.0) > 1e-10:
        print("WARNING: PMF not properly normalized")
    
    if len(pmf) == 0:
        print("WARNING: Empty PMF")
```

### Common Integration Issues

#### Dependency Resolution Strategies

```bash
# Diagnose dependency conflicts
pip check
pip list | grep -E "(numpy|pandas|scipy|matplotlib)"

# Resolve version conflicts
pip install --upgrade pip setuptools wheel
pip install -r requirements.txt --force-reinstall
```

## Conclusion

This developer guide provides the foundation for contributing to the Think Bayes 2 project while maintaining its educational and scientific integrity. The development approach emphasizes clarity, correctness, and pedagogical value in every contribution.

Remember that Think Bayes 2 serves both as a computational tool and an educational resource. Every code change should enhance understanding while maintaining mathematical rigor. The testing framework, documentation standards, and review processes all support this dual mission.

As you contribute to the project, consider both the immediate functional requirements and the broader educational context. The most valuable contributions improve both the computational capabilities and the learning experience for future users.

---

*For questions about development practices or contribution guidelines, consult the project's GitHub repository or contact the development team through the appropriate channels.*