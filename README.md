# Think Bayes 2

by Allen B. Downey

[The HTML version of this book is here](http://allendowney.github.io/ThinkBayes2).

*Think Bayes* is an introduction to Bayesian statistics using computational methods.  

*Think Bayes* is a Free Book. It is available under the [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)](https://creativecommons.org/licenses/by-nc-sa/4.0/), which means that you are free to copy and modify it, as long as you attribute the work and don't use it for commercial purposes.

Other Free Books by Allen Downey are available from [Green Tea Press](https://greenteapress.com/wp/).

## Installation

This project now uses **Python 3.13+** and **uv** for package management.

### Prerequisites

1. Install Python 3.13 or later
2. Install [uv](https://docs.astral.sh/uv/getting-started/installation/):
   ```bash
   curl -LsSf https://astral.sh/uv/install.sh | sh
   ```

### Quick Setup

```bash
# Clone the repository
git clone https://github.com/wilsonify/ThinkBayes2.git
cd ThinkBayes2

# Create virtual environment and install all dependencies
make create_environment

# Activate the environment
source .venv/bin/activate
```

### Alternative Installation Methods

#### Using uv directly

```bash
# Create virtual environment
uv venv --python 3.13

# Install all dependencies (including notebooks)
uv pip install -e .[all]

# Or install just core dependencies
uv pip install -e .

# Or install with development tools
uv pip install -e .[dev]
```

#### Using pip (legacy)

```bash
# Install core dependencies
pip install -r requirements.txt

# Install development dependencies
pip install -r requirements-dev.txt

# Install the package in development mode
pip install -e src/think-bayes
```

### Development Setup

```bash
# Install development dependencies
make install-dev

# Run linting
make lint

# Format code
make format

# Run tests
make test
```


## Interactive Web Demo

 **New!** Try the interactive web demo with hands-on Bayesian examples:

### Quick Start
```bash
cd demo
npm install
npm run dev
```

Then open `http://localhost:5173` in your browser.

### Available Demos
- **Hockey Goals Analysis** - Gamma-Poisson modeling for sports analytics
- **Survival Analysis** - Weibull distribution and reliability modeling  
- **Changepoint Detection** - Bayesian detection of structural changes
- **Hospital Birth Rates** - Hierarchical models for medical statistics
- **Radiation Sensor Analysis** - Poisson-Gamma models for physical systems
- **Typos Estimation** - Capture-recapture methods for population estimation

Each demo features interactive controls, real-time visualizations, and mathematical frameworks to help you understand Bayesian concepts through hands-on experimentation.

 **Full documentation**: See `demo/README.md` for detailed setup instructions and troubleshooting.

## Run the notebooks

* [Chapter 11](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap11.ipynb)

* [Chapter 12](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap12.ipynb)

* [Chapter 13](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap13.ipynb)

* [Chapter 14](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap14.ipynb)

* [Chapter 15](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap15.ipynb)

* [Chapter 16](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap16.ipynb)

* [Chapter 17](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap17.ipynb)

* [Chapter 18](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap18.ipynb)

* [Chapter 19](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap19.ipynb)

* [Chapter 20](https://colab.research.google.com/github/AllenDowney/ThinkBayes2/blob/master/notebooks/chap20.ipynb)
