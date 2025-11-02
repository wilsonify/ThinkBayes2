# Think Bayes 2 - Interactive Demo

An interactive frontend application demonstrating key concepts from Allen B. Downey's "Think Bayes 2" book. This web app provides hands-on examples of Bayesian statistics with visualizations and interactive calculations.

## Features

### 🎯 Interactive Demonstrations
- **Bayes' Theorem Calculator**: Adjust parameters and see real-time probability updates
- **Cookie Problem**: Classic Bayesian inference example with configurable bowls
- **Dice Problem**: Simulate dice rolls and update beliefs about which die was chosen
- **Probability Distributions**: Explore PMFs and CDFs for binomial, Poisson, and uniform distributions

### 📊 Visualizations
- Real-time probability charts using Recharts
- Interactive sliders for parameter adjustment
- Color-coded probability updates
- Mathematical notation with KaTeX

### 🎨 Modern UI
- Clean, responsive design with Tailwind CSS
- Intuitive navigation between sections
- Professional color scheme and typography
- Mobile-friendly interface

## Technologies Used

- **React 18** - Modern component-based UI framework
- **Vite** - Fast development build tool
- **Tailwind CSS** - Utility-first CSS framework
- **Recharts** - Chart library for data visualization
- **KaTeX** - Mathematical notation rendering
- **Lucide React** - Beautiful icon set

## Quick Start

1. **Install dependencies**:
   ```bash
   cd demo
   npm install
   ```

2. **Start development server**:
   ```bash
   npm run dev
   ```

3. **Open your browser** and navigate to `http://localhost:3000`

## Build for Production

```bash
npm run build
```

The built files will be in the `dist` directory.

## Project Structure

```
demo/
├── src/
│   ├── components/
│   │   ├── Header.jsx           # App header with branding
│   │   ├── Navigation.jsx       # Section navigation
│   │   ├── Introduction.jsx     # Welcome and overview
│   │   ├── BayesTheorem.jsx     # Interactive Bayes' theorem demo
│   │   ├── CookieProblem.jsx    # Classic cookie bowl problem
│   │   ├── DiceProblem.jsx      # Dice inference simulation
│   │   └── PMFDemo.jsx          # Probability distributions explorer
│   ├── App.jsx                  # Main application component
│   ├── main.jsx                 # Application entry point
│   └── index.css                # Global styles
├── index.html                   # HTML template
├── package.json                 # Dependencies and scripts
├── vite.config.js               # Vite configuration
├── tailwind.config.js           # Tailwind CSS configuration
└── postcss.config.js            # PostCSS configuration
```

## Learning Objectives

This demo helps users understand:

1. **Bayesian Thinking**: How prior beliefs are updated with evidence
2. **Conditional Probability**: The foundation of Bayesian inference
3. **Prior vs Posterior**: How evidence changes our beliefs
4. **Likelihood Functions**: How evidence relates to hypotheses
5. **Probability Distributions**: The building blocks of Bayesian analysis

## Examples Included

### Medical Testing Scenario
- Demonstrates counterintuitive probability results
- Shows importance of considering base rates
- Interactive parameter adjustment

### Cookie Bowl Problem
- Classic example from the book
- Visual representation of probability updates
- Configurable bowl compositions

### Dice Inference
- Multi-step Bayesian updating
- Simulation of evidence accumulation
- Real-time belief visualization

### Distribution Explorer
- Binomial, Poisson, and Uniform distributions
- Interactive parameter controls
- PMF and CDF visualizations

## Educational Value

This interactive demo serves as a supplement to the Think Bayes 2 book by:

- Providing hands-on experience with Bayesian concepts
- Visualizing abstract probability ideas
- Allowing experimentation with different parameters
- Reinforcing key learning objectives through practice

## Contributing

This demo is designed to accompany the Think Bayes 2 book. For contributions to the main book content, please visit the [official repository](https://github.com/AllenDowney/ThinkBayes2).

## License

This demo follows the same license as Think Bayes 2: Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0).

## Resources

- [Think Bayes 2 Book](http://allendowney.github.io/ThinkBayes2)
- [Official Repository](https://github.com/AllenDowney/ThinkBayes2)
- [React Documentation](https://react.dev)
- [Tailwind CSS](https://tailwindcss.com)
