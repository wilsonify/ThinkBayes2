import pandas as pd
from scipy.stats import norm
from empiricaldist import Pmf
import statsmodels.formula.api as smf
import numpy as np

# Load data
def load_and_prepare_data(file_path):
    """
    Load the dataset and prepare it for analysis.
    """
    df = pd.read_csv(file_path, parse_dates=["DATE"])
    df["YEAR"] = df["DATE"].dt.year
    snow = df.groupby("YEAR")["SNOW"].sum()
    return snow

# Create a Pmf for snowfall data
def create_pmf(snow):
    """
    Create a probability mass function (Pmf) for snowfall data.
    """
    return Pmf.from_seq(snow)

# Fit a least-squares regression model
def fit_regression(data):
    """
    Fit a least-squares regression model to the snowfall data.
    """
    offset = data["YEAR"].mean().round()
    data["x"] = data["YEAR"] - offset
    data["y"] = data["SNOW"]
    formula = "y ~ x"
    results = smf.ols(formula, data=data).fit()
    return results, offset

# Create priors
def make_uniform(qs, label):
    """
    Create a uniform prior distribution.
    """
    probs = np.ones_like(qs) / len(qs)
    return Pmf(dict(zip(qs, probs)), name=label)

def create_priors():
    """
    Create prior distributions for slope, intercept, and sigma.
    """
    slope_qs = np.linspace(-0.5, 1.5, 51)
    intercept_qs = np.linspace(54, 75, 41)
    sigma_qs = np.linspace(20, 35, 31)
    prior_slope = make_uniform(slope_qs, "Slope")
    prior_intercept = make_uniform(intercept_qs, "Intercept")
    prior_sigma = make_uniform(sigma_qs, "Sigma")
    return prior_slope, prior_intercept, prior_sigma

# Make a joint distribution
def make_joint(pmf1, pmf2):
    """
    Create a joint distribution from two PMFs.
    """
    return Pmf({(x, y): p1 * p2 for x, p1 in pmf1.items() for y, p2 in pmf2.items()})

def make_joint3(pmf1, pmf2, pmf3):
    """
    Create a joint distribution from three PMFs.
    """
    joint2 = make_joint(pmf2, pmf1)
    joint3 = make_joint(pmf3, joint2)
    return Pmf(joint3)

# Compute likelihood
def compute_likelihood(xs, ys, prior):
    """
    Compute the likelihood of the data for each set of parameters.
    """
    likelihood = prior.copy()
    for (slope, intercept, sigma) in prior.index:
        expected = slope * xs + intercept
        resid = ys - expected
        densities = norm.pdf(resid, 0, sigma)
        likelihood[slope, intercept, sigma] = densities.prod()
    return likelihood

# Bayesian update
def update_posterior(prior, likelihood):
    """
    Perform Bayesian update to compute the posterior.
    """
    posterior = prior * likelihood
    posterior.normalize()
    return posterior

# Main function
def main():
    # Load and prepare data
    snow = load_and_prepare_data("/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv")
    pmf_snowfall = create_pmf(snow)
    mean, std = pmf_snowfall.mean(), pmf_snowfall.std()
    print(f"Mean snowfall: {mean}, Std dev: {std}")

    # Fit regression model
    data = snow.reset_index()
    results, offset = fit_regression(data)
    print(f"Regression coefficients: {results.params}")
    print(f"Residual std dev: {results.resid.std()}")

    # Create priors
    prior_slope, prior_intercept, prior_sigma = create_priors()
    prior = make_joint3(prior_slope, prior_intercept, prior_sigma)

    # Compute likelihood and update posterior
    xs = data["x"]
    ys = data["y"]
    likelihood = compute_likelihood(xs, ys, prior)
    posterior = update_posterior(prior, likelihood)

    # Extract marginals
    posterior_slope = posterior.marginal(0)
    posterior_intercept = posterior.marginal(1)
    posterior_sigma = posterior.marginal(2)

    print("Posterior slope mean:", posterior_slope.mean())
    print("Posterior intercept mean:", posterior_intercept.mean())
    print("Posterior sigma mean:", posterior_sigma.mean())

if __name__ == "__main__":
    main()
