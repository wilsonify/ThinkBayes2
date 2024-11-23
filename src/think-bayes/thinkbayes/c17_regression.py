from dataclasses import dataclass

import numpy as np
import pandas as pd
import statsmodels.formula.api as smf
from empiricaldist import Pmf
from matplotlib import pyplot as plt
from matplotlib.gridspec import GridSpec
from scipy.stats import norm


@dataclass
class RegressionModel:
    """
    Dataclass representing a regression model.

    Attributes:
        intercept (np.float64): The intercept of the regression line.
        offset (np.float64): The offset value for the model.
        sigma (np.float64): The standard deviation (sigma) of the model.
        slope (np.float64): The slope of the regression line.
    """
    intercept: np.float64
    offset: np.float64
    sigma: np.float64
    slope: np.float64

    def predict(self, x):
        return self.slope * x + self.intercept + self.sigma + self.offset


def normalize(joint):
    """Normalize a joint distribution.

    joint: DataFrame
    """
    prob_data = joint.to_numpy().sum()
    joint /= prob_data
    return prob_data


# Load data
def load_data(file_path):
    """
    Load the dataset and prepare it for analysis.
    """
    df = pd.read_csv(file_path, parse_dates=["DATE"])
    return df


def prepare_data(df):
    df["YEAR"] = df["DATE"].dt.year
    snow = df.groupby("YEAR")["SNOW"].sum()
    return snow


# Fit a least-squares regression model
def fit_regression(data):
    """
    Fit a least-squares regression model to the snowfall data.
    """
    offset = pd.Series(data.index.values).mean().round()
    x = data.index - offset
    y = data.values
    formula = "y ~ x"
    results = smf.ols(formula, data=pd.DataFrame({"x": x, "y": y})).fit()
    model = RegressionModel(
        offset=offset,
        intercept=results.params.Intercept,
        slope=results.params.x,
        sigma=results.resid.std()
    )
    return model


# Create priors
def make_uniform(qs):
    """Make a Pmf that represents a uniform distribution."""
    pmf = Pmf(1.0, qs)
    pmf.normalize()
    return pmf


def make_joint3(pmf1: Pmf, pmf2: Pmf, pmf3: Pmf):
    """Make a joint distribution with three parameters."""
    joint2 = pmf1.make_joint(pmf2)
    joint3 = joint2.make_joint(pmf3)
    return joint3


# Compute likelihood
def compute_likelihood(data, model: RegressionModel):
    """
    Compute the likelihood of the data for each set of parameters.
    """
    offset = model.offset
    inter = model.intercept
    slope = model.slope
    sigma = model.sigma
    expected = slope * data.index + inter
    resid = data.values - expected
    densities = norm(0, sigma).pdf(resid)
    likelihood = densities.prod()
    return likelihood


# Bayesian update
def update_posterior(data, prior):
    """
    Perform Bayesian update to compute the posterior.
    """
    xs = data['x']
    ys = data['y']
    likelihood = prior.copy()
    for slope, inter, sigma in prior.index:
        expected = slope * xs + inter
        resid = ys - expected
        densities = norm.pdf(resid, 0, sigma)
        likelihood[slope, inter, sigma] = densities.prod()
    posterior = prior * likelihood
    posterior.normalize()
    return posterior


def update_optimized(data, prior):
    """Posterior distribution of regression parameters
    `slope`, `inter`, and `sigma`.

    prior: Pmf representing the joint prior
    data: DataFrame with columns `x` and `y`

    returns: Pmf representing the joint posterior
    """
    xs = data['x']
    ys = data['y']
    sigmas = prior.columns
    likelihood = prior.copy()

    for slope, inter in prior.index[0, 1]:
        expected = slope * xs + inter
        resid = ys - expected
        resid_mesh, sigma_mesh = np.meshgrid(resid, sigmas)
        densities = norm.pdf(resid_mesh, 0, sigma_mesh)
        likelihood.loc[slope, inter] = densities.prod(axis=1)

    posterior = prior * likelihood
    normalize(posterior)
    return posterior


# Main function
def main():
    # Load and prepare data
    snow = prepare_data(load_data("/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv"))
    pmf_snowfall = Pmf.from_seq(snow)
    mean, std = pmf_snowfall.mean(), pmf_snowfall.std()
    print(f"Mean snowfall: {mean}, Std dev: {std}")
    dist = norm(mean, std)
    qs = pmf_snowfall.qs
    ps = dist.cdf(qs)
    fig = plt.figure(figsize=(8, 5))
    gs = GridSpec(1, 1, height_ratios=[1])
    ax0 = fig.add_subplot(gs[0, 0])
    pmf_snowfall.make_cdf().plot(label='data', ax=ax0)
    ax0.plot(qs, ps, color='C5', label='model', )
    ax0.set_xlabel('Total snowfall (inches)')
    ax0.set_ylabel('CDF')
    ax0.set_title('Normal model of variation in snowfall')
    ax0.legend()
    plt.savefig("normal_model_of_variation_in_snowfall.png")

    # Fit regression model
    data = snow.reset_index()
    data.head(3)
    results = fit_regression(data)
    print(f"results = {results}")

    # Create priors
    slope_qs = np.linspace(-0.5, 1.5, 51)
    prior_slope = make_uniform(slope_qs, "Slope")

    intercept_qs = np.linspace(54, 75, 41)
    prior_intercept = make_uniform(intercept_qs, "Intercept")

    sigma_qs = np.linspace(20, 35, 31)
    prior_sigma = make_uniform(sigma_qs, "Sigma")

    prior = make_joint3(prior_slope, prior_intercept, prior_sigma)
    print(f"prior = {prior}")

    # Compute likelihood and update posterior
    likelihood = compute_likelihood(data, results)
    print(f"likelihood = {likelihood}")

    posterior = update_posterior(data, prior)
    print(f"posterior = {posterior}")

    # Extract marginals
    model_updated = {
        "offset": results["offset"],
        "intercept": posterior.marginal(1).mode(),
        "slope": posterior.marginal(0).mode(),
        "sigma": posterior.marginal(2).mode()
    }
    print(f"posterior model = {model_updated}")


if __name__ == "__main__":
    main()
