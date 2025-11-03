# Interactive Demo Guide

This guide provides comprehensive documentation for the Think Bayes 2 interactive web demo.

## Overview

The interactive demo is a modern React-based web application that brings the concepts from Think Bayes 2 to life through hands-on experimentation. Each demo features:

- **Interactive Controls**: Sliders, inputs, and buttons to adjust parameters
- **Real-time Visualizations**: Charts that update instantly as you change parameters
- **Mathematical Frameworks**: Detailed explanations of the underlying Bayesian models
- **Educational Context**: Clear learning objectives and practical applications

## Getting Started

### Prerequisites

- **Node.js 16+**: Required for the development environment
- **Modern Web Browser**: Chrome, Firefox, Safari, or Edge
- **Basic Command Line Knowledge**: For running the setup commands

### Quick Setup

#### Automated Setup (Recommended)

**macOS/Linux:**
```bash
cd demo
./setup.sh
```

**Windows:**
```cmd
cd demo
setup.bat
```

#### Manual Setup

```bash
cd demo
npm install
npm run dev
```

Then open `http://localhost:5173` in your browser.

## Available Demos

### 1. Hockey Goals Analysis

**Learning Objective**: Understand Bayesian inference for count data using hierarchical models.

**Key Concepts**:
- Gamma-Poisson distribution
- Prior and posterior distributions
- Hierarchical Bayesian modeling
- Sports analytics applications

**Interactive Features**:
- Add/remove games and adjust goal counts
- Modify prior parameters (α, β)
- Visualize prior, likelihood, and posterior distributions
- See real-time posterior mean calculations

**Mathematical Model**:
```
goals ~ Poisson(λ)
λ ~ Gamma(α, β)
```

### 2. Survival Analysis

**Learning Objective**: Learn time-to-event analysis using Weibull distributions.

**Key Concepts**:
- Survival functions and hazard rates
- Weibull distribution parameters
- Kaplan-Meier estimation
- Reliability modeling

**Interactive Features**:
- Input survival time data
- Adjust Weibull shape and scale parameters
- Visualize survival curves and hazard functions
- Compare different parameter scenarios

**Mathematical Model**:
```
T ~ Weibull(k, λ)
S(t) = exp(-(t/λ)^k)
h(t) = (k/λ) * (t/λ)^(k-1)
```

### 3. Changepoint Detection

**Learning Objective**: Detect structural changes in time series data using Bayesian methods.

**Key Concepts**:
- Poisson process modeling
- Changepoint probability distributions
- Time series analysis
- Quality control applications

**Interactive Features**:
- Input time series event data
- Adjust prior parameters for different regimes
- Visualize changepoint probability distribution
- Identify most likely changepoint locations

**Mathematical Model**:
```
events_t ~ Poisson(λ₁) for t ≤ τ
events_t ~ Poisson(λ₂) for t > τ
τ ~ Uniform(1, n-1)
```

### 4. Hospital Birth Rates

**Learning Objective**: Understand hierarchical Bayesian modeling for multi-level data.

**Key Concepts**:
- Hierarchical models and partial pooling
- Hospital performance comparison
- Multi-level Bayesian inference
- Medical statistics applications

**Interactive Features**:
- Add/remove hospitals and adjust birth data
- Modify hyperparameters for the population distribution
- Visualize hospital-specific and population-level estimates
- Compare complete vs. partial pooling

**Mathematical Model**:
```
births_i ~ Binomial(n_i, p_i)
p_i ~ Beta(α, β)
α, β ~ Hyperpriors
```

### 5. Radiation Sensor Analysis

**Learning Objective**: Apply Bayesian inference to physical measurement systems.

**Key Concepts**:
- Poisson-Gamma conjugate models
- Sensor calibration and efficiency
- Source strength estimation
- Physical measurement uncertainty

**Interactive Features**:
- Input radiation sensor readings
- Adjust source rate, efficiency, and background parameters
- Visualize posterior distributions for all parameters
- Generate predictive distributions for future measurements

**Mathematical Model**:
```
counts ~ Poisson(source * efficiency + background)
source ~ Gamma(α₁, β₁)
efficiency ~ Beta(α₂, β₂)
```

### 6. Typos Estimation (Capture-Recapture)

**Learning Objective**: Learn capture-recapture methods for population estimation.

**Key Concepts**:
- Lincoln index estimation
- Bayesian capture-recapture models
- Population size estimation
- Ecological and quality control applications

**Interactive Features**:
- Adjust typo counts found by two readers
- Modify shared typo observations
- Visualize posterior distribution of total typos
- Compare Lincoln index vs. Bayesian estimates

**Mathematical Model**:
```
N̂ = (n₁ × n₂) / k  (Lincoln Index)
P(N|data) ∝ P(data|N) × P(N)
```

## Educational Features

### Mathematical Frameworks

Each demo includes a "Mathematical Framework" section that explains:

- **Model Structure**: The statistical model and its components
- **Prior Distributions**: How initial beliefs are represented
- **Likelihood Functions**: How data relates to parameters
- **Posterior Inference**: How beliefs are updated with evidence
- **Key Assumptions**: Important model assumptions and limitations

### Interactive Visualizations

All demos feature real-time charts built with Recharts:

- **Distribution Plots**: Prior, posterior, and likelihood distributions
- **Time Series Charts**: Sequential data and trend analysis
- **Scatter Plots**: Observed vs. expected values
- **Bar Charts**: Comparison scenarios and categorical data
- **Area Charts**: Probability density and cumulative distributions

### Parameter Controls

Interactive controls allow users to:

- **Adjust Parameters**: Sliders and number inputs for model parameters
- **Modify Data**: Add/remove data points and change observations
- **Switch Scenarios**: Toggle between different example scenarios
- **Reset Values**: Return to default parameter settings

## Technical Implementation

### Architecture

The demo is built with modern web technologies:

- **React 18**: Component-based UI framework
- **Vite**: Fast development build tool with hot reload
- **Tailwind CSS**: Utility-first CSS framework
- **Recharts**: Declarative chart library
- **KaTeX**: Mathematical notation rendering
- **Vitest**: Unit testing framework

### Component Structure

Each demo follows a consistent structure:

```
ComponentName.jsx
├── State management (useState)
├── Bayesian calculations (useMemo)
├── Helper functions
├── Event handlers
├── JSX rendering
└── Mathematical framework section
```

### Testing

All demos include comprehensive unit tests:

- **Rendering Tests**: Verify components render without errors
- **Calculation Tests**: Check Bayesian computations
- **Interaction Tests**: Test user controls and updates
- **Integration Tests**: Verify complete workflows

Run tests with:
```bash
npm test                    # All tests
npm test -- --run ComponentName.test.jsx  # Specific component
```

## Troubleshooting

### Common Issues

1. **404 Errors**: All demos are now fixed. Ensure you're running the latest version
2. **Port Conflicts**: The dev server automatically finds available ports
3. **Dependency Issues**: Run `npm install` to refresh dependencies
4. **Build Failures**: Check for syntax errors and run tests to identify issues

### Performance Tips

- Use `npm run dev` for development with hot reload
- Run `npm test` to verify all components work
- Use `npm run build` for optimized production builds
- Clear browser cache if UI elements don't update

### Browser Compatibility

The demo works best with modern browsers:
- **Chrome 90+**: Full support
- **Firefox 88+**: Full support  
- **Safari 14+**: Full support
- **Edge 90+**: Full support

## Contributing

### Adding New Demos

1. **Create Component**: `src/components/NewDemo.jsx`
2. **Add Tests**: `src/test/NewDemo.test.jsx`
3. **Update Navigation**: Add to `src/App.jsx`
4. **Document**: Update README files
5. **Test**: Verify all tests pass

### Code Style

- Follow React functional component patterns
- Use `useMemo` for expensive calculations
- Implement proper error handling
- Include mathematical framework sections
- Add comprehensive unit tests

### Pull Request Process

1. Fork the repository
2. Create feature branch
3. Add demo with tests
4. Update documentation
5. Submit pull request with description

## Resources

### Documentation

- **[Demo README](../demo/README.md)**: Setup and usage instructions
- **[Main Repository](https://github.com/AllenDowney/ThinkBayes2)**: Book source code
- **[React Documentation](https://react.dev)**: React framework guide
- **[Vite Documentation](https://vitejs.dev)**: Build tool documentation

### Learning Materials

- **[Think Bayes 2 Book](http://allendowney.github.io/ThinkBayes2)**: Original textbook
- **[Bayesian Methods Course](https://www.coursera.org/learn/bayesian-statistics)**: Online course
- **[Statistical Rethinking](https://xcelab.net/rm/statistical-rethinking/)**: Advanced Bayesian methods

### Community

- **[GitHub Issues](https://github.com/AllenDowney/ThinkBayes2/issues)**: Bug reports and feature requests
- **[Discussions](https://github.com/AllenDowney/ThinkBayes2/discussions)**: Questions and answers
- **[Stack Overflow](https://stackoverflow.com/questions/tagged/bayesian)**: Bayesian statistics help

---

**🎉 Start exploring the demos to learn Bayesian statistics through interactive experimentation!**
