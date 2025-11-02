# User Guide

## Introduction

This guide serves as your companion for working through Think Bayes 2, providing practical guidance for implementing Bayesian statistical methods using computational tools. Whether you're a student encountering Bayesian statistics for the first time or a practitioner seeking to apply these methods to real problems, this documentation will help you navigate the computational landscape of Bayesian analysis.

The approach taken in Think Bayes 2 emphasizes learning by doing. Rather than presenting abstract mathematical formulas without context, we implement concepts directly in code, allowing you to see how probability distributions update, how evidence accumulates, and how predictions emerge from Bayesian inference. This hands-on methodology builds intuition that extends beyond the specific examples covered in the text.

## Installation and Setup

### System Requirements

Before beginning your journey with Think Bayes 2, ensure your system meets the following requirements:

- **Python Version**: 3.10 or higher (3.12+ recommended for optimal performance)
- **Memory**: Minimum 4GB RAM, 8GB+ recommended for larger datasets
- **Storage**: 2GB available space for dependencies and examples
- **Operating System**: Windows 10+, macOS 10.15+, or Linux (Ubuntu 18.04+)

### Installation Process

The installation process has been designed to be straightforward and reproducible across different systems. Follow these steps carefully to establish your working environment.

#### Step 1: Environment Preparation

We strongly recommend using a virtual environment to isolate dependencies and prevent conflicts with existing Python installations:

```bash
# Create a dedicated virtual environment
python -m venv thinkbayes-env

# Activate the environment
# On Windows:
thinkbayes-env\Scripts\activate
# On macOS/Linux:
source thinkbayes-env/bin/activate
```

#### Step 2: Dependency Installation

The project separates core dependencies from development tools, allowing you to install only what you need:

```bash
# Install core computational dependencies
pip install -r requirements.txt

# For interactive notebook work (recommended)
pip install jupyterlab ipywidgets

# Verify installation
python -c "import thinkbayes; print('Think Bayes successfully imported')"
```

#### Step 3: Download Example Materials

The book's examples and notebooks are available through multiple channels:

```bash
# Clone the complete repository (includes all materials)
git clone https://github.com/AllenDowney/ThinkBayes2.git
cd ThinkBayes2

# Or download only the notebooks
# Visit: https://github.com/AllenDowney/ThinkBayes2/raw/master/ThinkBayes2Notebooks.zip
```

### Verification and Testing

After installation, verify that everything functions correctly:

```python
# Test basic functionality
import thinkbayes as tb
import thinkplot as tp

# Create a simple probability mass function
pmf = tb.Pmf()
pmf.Set('A', 0.3)
pmf.Set('B', 0.7)
print(f"PMF created with {len(pmf)} elements")

# Test plotting capability
tp.Hist(pmf)
print("Plotting functionality verified")
```

## Core Concepts and Computational Approaches

### Understanding Probability Distributions

In Bayesian analysis, probability distributions serve as our fundamental building blocks for representing uncertainty. Think Bayes 2 provides several key classes for working with these distributions:

#### Probability Mass Functions (PMFs)

PMFs represent discrete probability distributions, assigning probabilities to specific outcomes. They form the foundation for many Bayesian analyses:

```python
import thinkbayes as tb

# Creating a PMF for a six-sided die
die = tb.Pmf()
for i in range(1, 7):
    die.Set(i, 1/6)

# PMF operations naturally support Bayesian updating
die.Mult(6, 2)  # Incorporate evidence: rolled a 6 twice
die.Normalize()  # Renormalize to maintain probability sum = 1
```

#### Cumulative Distribution Functions (CDFs)

CDFs provide an alternative representation that's particularly useful for certain types of analysis and visualization:

```python
# Convert PMF to CDF for percentile calculations
die_cdf = tb.MakeCdfFromPmf(die)
median = die_cdf.Percentile(50)
print(f"Median roll: {median}")
```

### Bayesian Inference Framework

The core of Bayesian analysis revolves around Bayes' theorem, which we implement computationally through a systematic process:

#### Prior Distribution Specification

Your analysis begins by encoding prior knowledge or assumptions into a probability distribution:

```python
# Example: Prior distribution for coin bias
coin_bias = tb.Beta(2, 2)  # Represents belief about fairness
# Beta(2,2) suggests slight preference for fairness but allows variation
```

#### Likelihood Function Implementation

The likelihood function quantifies how probable the observed data would be under different parameter values:

```python
def likelihood(data, hypothesis):
    """Calculate likelihood of observing data given hypothesis"""
    heads, tails = data
    bias = hypothesis
    return (bias ** heads) * ((1 - bias) ** tails)
```

#### Posterior Distribution Computation

Combining prior and likelihood yields the posterior distribution, which represents updated beliefs after observing data:

```python
# Update beliefs after observing 7 heads and 3 tails
data = (7, 3)
posterior = coin_bias.Update(data)
```

## Practical Examples and Case Studies

### Example 1: Dice Problem

The dice problem illustrates fundamental Bayesian concepts through a simple, intuitive scenario:

```python
import thinkbayes as tb
import thinkplot as tp

class Dice(tb.Suite):
    """Suite representing different dice types"""
    
    def Likelihood(self, data, hypo):
        """Likelihood of rolling 'data' given die type 'hypo'"""
        if hypo == 4:
            return 1/4 if data <= 4 else 0
        elif hypo == 6:
            return 1/6 if data <= 6 else 0
        elif hypo == 8:
            return 1/8 if data <= 8 else 0
        elif hypo == 12:
            return 1/12 if data <= 12 else 0
        elif hypo == 20:
            return 1/20 if data <= 20 else 0
        return 0

# Initialize with equal prior probabilities for each die type
dice = Dice([4, 6, 8, 12, 20])

# Update after observing a roll of 6
dice.Update(6)

# Visualize posterior beliefs
tp.Pmf(dice)
tp.Config(title='Posterior distribution for die types after rolling 6')
```

This example demonstrates several key Bayesian principles:

1. **Prior Specification**: Equal initial belief across all hypotheses
2. **Evidence Incorporation**: Systematic updating based on observed data
3. **Posterior Interpretation**: Quantified uncertainty about the true die type

### Example 2: Euro Coin Problem

A more sophisticated example involves estimating the bias of a European coin:

```python
class Euro(tb.Suite):
    """Suite for estimating coin bias"""
    
    def __init__(self, step=0.001):
        """Initialize with uniform prior over [0, 1]"""
        hypotheses = [i/1000 for i in range(0, 1001, int(step*1000))]
        super().__init__(hypotheses)
    
    def Likelihood(self, data, hypo):
        """Binomial likelihood for coin flips"""
        heads, total = data
        return hypo ** heads * (1 - hypo) ** (total - heads)

# Analysis of European coin data (140 heads, 110 tails)
euro = Euro(step=0.001)
euro.Update((140, 250))

# Calculate summary statistics
mean = euro.Mean()
credible_interval = euro.CredibleInterval(90)
print(f"Estimated bias: {mean:.3f}")
print(f"90% credible interval: {credible_interval}")
```

### Example 3: Bayesian Estimation with Real Data

Working with real datasets requires careful consideration of data quality, missing values, and domain-specific factors:

```python
import pandas as pd
import numpy as np

def analyze_survey_data(file_path):
    """Analyze survey responses using Bayesian methods"""
    
    # Load and prepare data
    data = pd.read_csv(file_path)
    
    # Handle missing responses
    responses = data['response'].dropna()
    
    # Define prior based on historical data
    prior_mean = 0.5  # Neutral prior
    prior_strength = 10  # Equivalent sample size
    
    # Create Beta prior
    prior = tb.Beta(prior_mean * prior_strength, 
                    (1 - prior_mean) * prior_strength)
    
    # Update with observed data
    positive_responses = (responses > 0).sum()
    total_responses = len(responses)
    
    posterior = prior.Update((positive_responses, total_responses))
    
    # Generate predictions
    predictions = posterior.Sample(1000)
    
    return {
        'posterior_mean': posterior.Mean(),
        'credible_interval': posterior.CredibleInterval(95),
        'predictions': predictions
    }
```

## Common Patterns and Best Practices

### Data Preparation Strategies

Effective Bayesian analysis requires thoughtful data preparation:

```python
def prepare_data(raw_data, continuous_threshold=10):
    """
    Prepare data for Bayesian analysis with appropriate handling
    of continuous and discrete variables
    """
    
    prepared = {}
    
    for column in raw_data.columns:
        data = raw_data[column].dropna()
        
        if data.nunique() <= continuous_threshold:
            # Discrete data: create PMF
            pmf = tb.Pmf()
            for value, count in data.value_counts().items():
                pmf.Set(value, count / len(data))
            prepared[column] = pmf
        else:
            # Continuous data: create empirical CDF
            cdf = tb.Cdf()
            for value in sorted(data):
                cdf.Set(value, data[data <= value].count() / len(data))
            prepared[column] = cdf
    
    return prepared
```

### Model Selection and Comparison

Bayesian methods provide natural frameworks for model comparison:

```python
def compare_models(data, models):
    """
    Compare competing models using Bayesian evidence
    """
    
    evidence = {}
    
    for model_name, model in models.items():
        # Calculate marginal likelihood (evidence)
        likelihood = model.MarginalLikelihood(data)
        evidence[model_name] = likelihood
    
    # Normalize to get model probabilities
    total_evidence = sum(evidence.values())
    model_probabilities = {name: ev/total_evidence 
                          for name, ev in evidence.items()}
    
    return model_probabilities
```

### Visualization Techniques

Effective communication of Bayesian results requires appropriate visualization:

```python
def create_bayesian_visualization(posterior, prior=None, title="Posterior Distribution"):
    """
    Create comprehensive visualization of Bayesian analysis results
    """
    
    import matplotlib.pyplot as plt
    
    fig, axes = plt.subplots(2, 2, figsize=(12, 10))
    fig.suptitle(title, fontsize=16)
    
    # Posterior distribution
    thinkplot.Pmf(posterior, ax=axes[0, 0])
    axes[0, 0].set_title('Posterior Distribution')
    
    # Credible interval
    thinkplot.Cdf(posterior.MakeCdf(), ax=axes[0, 1])
    axes[0, 1].set_title('Cumulative Distribution')
    
    # Posterior predictive checks
    predictions = posterior.Sample(1000)
    axes[1, 0].hist(predictions, bins=30, alpha=0.7)
    axes[1, 0].set_title('Posterior Predictive Distribution')
    
    # Summary statistics
    summary_text = f"Mean: {posterior.Mean():.3f}\n"
    summary_text += f"Median: {posterior.Percentile(50):.3f}\n"
    summary_text += f"95% CI: {posterior.CredibleInterval(95)}"
    axes[1, 1].text(0.1, 0.5, summary_text, transform=axes[1, 1].transAxes)
    axes[1, 1].set_title('Summary Statistics')
    
    plt.tight_layout()
    return fig
```

## Troubleshooting and Frequently Asked Questions

### Common Installation Issues

#### Problem: Import Errors After Installation

**Symptoms**: `ModuleNotFoundError` when trying to import `thinkbayes`

**Diagnostic Steps**:
1. Verify Python path includes the installation directory
2. Check that virtual environment is activated
3. Confirm installation in development mode

**Solutions**:
```bash
# Reinstall in development mode
pip install -e src/think-bayes
pip install -e src/think-plot

# Verify installation
python -c "import sys; print(sys.path)"
python -c "import thinkbayes; print(thinkbayes.__file__)"
```

#### Problem: Plotting Function Failures

**Symptoms**: Errors when using `thinkplot` functions

**Common Causes**:
- Missing matplotlib backend
- Display environment issues
- Version incompatibilities

**Resolution**:
```python
# Test plotting backend
import matplotlib
matplotlib.use('Agg')  # Non-interactive backend for testing
import thinkplot as tp

# Verify basic plotting
pmf = tb.Pmf()
pmf.Set('test', 1.0)
tp.Pmf(pmf)
```

### Computational Performance Issues

#### Problem: Slow Convergence in Complex Models

**Analysis**: Complex models with many parameters may require specialized optimization strategies

**Approaches**:
```python
# Use conjugate priors when possible
def efficient_beta_update(alpha, beta, data):
    """Analytical update for Beta-Binomial model"""
    heads, tails = data
    return alpha + heads, beta + tails

# Implement adaptive sampling for complex posteriors
def adaptive_sampling(posterior, n_samples=10000, tolerance=0.01):
    """Adaptive sampling strategy for efficient posterior exploration"""
    
    samples = []
    current_estimate = posterior.Mean()
    convergence = float('inf')
    
    while len(samples) < n_samples and convergence > tolerance:
        new_sample = posterior.Sample()
        samples.append(new_sample)
        
        if len(samples) % 100 == 0:
            new_estimate = np.mean(samples[-100:])
            convergence = abs(new_estimate - current_estimate)
            current_estimate = new_estimate
    
    return samples
```

### Conceptual Clarifications

#### Question: How Do I Choose Appropriate Priors?

**Guidance**: Prior selection depends on available information and analysis goals:

1. **Uninformative Priors**: When minimal prior knowledge exists
2. **Informative Priors**: When domain expertise or previous research exists
3. **Weakly Informative Priors**: Regularization without strong constraints

```python
# Examples of different prior choices
def create_priors(prior_type="weakly_informative"):
    """Demonstrate different prior specification strategies"""
    
    if prior_type == "uninformative":
        # Uniform prior over reasonable range
        return tb.Pmf({i: 1/100 for i in range(1, 101)})
    
    elif prior_type == "informative":
        # Based on previous research findings
        prior = tb.Pmf()
        prior.Set(0.3, 0.1)   # Historical mode
        prior.Set(0.5, 0.6)   # Strong belief in fairness
        prior.Set(0.7, 0.1)   # Historical alternative
        prior.Normalize()
        return prior
    
    elif prior_type == "weakly_informative":
        # Gentle regularization toward center
        return tb.Beta(2, 2)  # Slight preference for middle values
```

#### Question: How Do I Interpret Credible Intervals?

**Explanation**: Credible intervals represent uncertainty in parameter estimates:

```python
def interpret_intervals(posterior, confidence_levels=[0.5, 0.8, 0.95]):
    """Demonstrate interpretation of different credible intervals"""
    
    interpretation = {}
    
    for level in confidence_levels:
        interval = posterior.CredibleInterval(level)
        interpretation[f"{int(level*100)}%"] = {
            'interval': interval,
            'interpretation': f"There is a {level*100}% probability that the true "
                            f"parameter lies between {interval[0]:.3f} and "
                            f"{interval[1]:.3f}",
            'width': interval[1] - interval[0]
        }
    
    return interpretation
```

## Advanced Topics and Extensions

### Hierarchical Modeling

Hierarchical models allow for partial pooling of information across groups:

```python
class HierarchicalModel:
    """Implementation of hierarchical Bayesian model"""
    
    def __init__(self, n_groups):
        self.n_groups = n_groups
        self.group_parameters = []
        self.hyperparameters = tb.Beta(1, 1)  # Hyperprior
    
    def fit(self, group_data):
        """Fit hierarchical model to grouped data"""
        
        for group_data_i in group_data:
            # Group-specific parameters informed by hyperparameters
            group_prior = tb.Beta(
                self.hyperparameters.alpha * 10,
                self.hyperparameters.beta * 10
            )
            group_posterior = group_prior.Update(group_data_i)
            self.group_parameters.append(group_posterior)
        
        # Update hyperparameters based on group estimates
        self._update_hyperparameters()
    
    def _update_hyperparameters(self):
        """Update hyperparameters based on group-level evidence"""
        # Implementation depends on specific hierarchical structure
        pass
```

### Model Checking and Validation

Robust Bayesian analysis requires thorough model checking:

```python
def posterior_predictive_check(model, observed_data, n_simulations=1000):
    """
    Perform posterior predictive checks to assess model adequacy
    """
    
    # Generate posterior predictive samples
    predictive_samples = []
    for _ in range(n_simulations):
        sample = model.PosteriorPredictiveSample()
        predictive_samples.append(sample)
    
    # Compare summary statistics
    observed_statistic = np.mean(observed_data)
    simulated_statistics = [np.mean(sample) for sample in predictive_samples]
    
    # Calculate posterior predictive p-value
    p_value = np.mean(np.array(simulated_statistics) >= observed_statistic)
    
    return {
        'observed_statistic': observed_statistic,
        'simulated_statistics': simulated_statistics,
        'posterior_predictive_p_value': p_value,
        'model_adequacy': 'adequate' if 0.1 < p_value < 0.9 else 'questionable'
    }
```

## Conclusion

This user guide provides the foundation for your journey into Bayesian statistical analysis using Think Bayes 2. The computational approach emphasizes understanding through implementation, allowing you to develop deep intuition about probability, inference, and uncertainty quantification.

Remember that Bayesian analysis is inherently iterative—your understanding will deepen as you work through examples, encounter challenges, and refine your approach. The methods presented here scale from simple textbook problems to complex real-world applications, always maintaining the core principles of coherent probabilistic reasoning.

As you progress through the book, don't hesitate to experiment with the examples, modify the code, and apply these techniques to problems that interest you. The true power of Bayesian methods emerges when you adapt them to your specific analytical challenges and domain expertise.

---

*This guide accompanies Think Bayes 2 and is intended to be used alongside the book's examples and exercises. For additional support, consult the project's GitHub repository and community forums.*