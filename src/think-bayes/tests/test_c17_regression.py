import pytest
import pandas as pd
import numpy as np
from scipy.stats import norm
from empiricaldist import Pmf
from regression_script import (  # Replace 'regression_script' with the name of your script file.
    load_and_prepare_data,
    create_pmf,
    fit_regression,
    make_uniform,
    create_priors,
    make_joint3,
    compute_likelihood,
    update_posterior,
)

@pytest.fixture
def sample_data():
    """Fixture for test data."""
    data = {
        "DATE": pd.date_range(start="1967-01-01", periods=5, freq="Y"),
        "SNOW": [10, 20, 30, 25, 35],
    }
    df = pd.DataFrame(data)
    return df

def test_load_and_prepare_data(sample_data, tmp_path):
    """Test loading and preparing data."""
    file_path = tmp_path / "test.csv"
    sample_data.to_csv(file_path, index=False)
    snow = load_and_prepare_data(file_path)
    assert snow.index[0] == 1967
    assert snow.iloc[0] == 10
    assert snow.sum() == 120

def test_create_pmf(sample_data):
    """Test creating a PMF from snowfall data."""
    snow = sample_data.groupby(sample_data["DATE"].dt.year)["SNOW"].sum()
    pmf = create_pmf(snow)
    assert isinstance(pmf, Pmf)
    assert np.isclose(pmf.mean(), snow.mean())
    assert np.isclose(pmf.std(), snow.std())

def test_fit_regression(sample_data):
    """Test fitting a regression model."""
    snow = sample_data.groupby(sample_data["DATE"].dt.year)["SNOW"].sum().reset_index()
    results, offset = fit_regression(snow)
    assert "x" in snow.columns
    assert "y" in snow.columns
    assert isinstance(offset, float)
    assert "x" in results.params.index

def test_make_uniform():
    """Test creating a uniform prior."""
    qs = np.linspace(-0.5, 1.5, 51)
    prior = make_uniform(qs, "Slope")
    assert isinstance(prior, Pmf)
    assert np.isclose(prior.sum(), 1)
    assert len(prior) == len(qs)

def test_create_priors():
    """Test creating priors for slope, intercept, and sigma."""
    prior_slope, prior_intercept, prior_sigma = create_priors()
    assert prior_slope.name == "Slope"
    assert prior_intercept.name == "Intercept"
    assert prior_sigma.name == "Sigma"
    assert len(prior_slope) == 51
    assert len(prior_intercept) == 41
    assert len(prior_sigma) == 31

def test_make_joint3():
    """Test creating a joint distribution."""
    slope_qs = np.linspace(-0.5, 1.5, 51)
    intercept_qs = np.linspace(54, 75, 41)
    sigma_qs = np.linspace(20, 35, 31)
    prior_slope = make_uniform(slope_qs, "Slope")
    prior_intercept = make_uniform(intercept_qs, "Intercept")
    prior_sigma = make_uniform(sigma_qs, "Sigma")
    joint = make_joint3(prior_slope, prior_intercept, prior_sigma)
    assert isinstance(joint, Pmf)
    assert len(joint) == len(slope_qs) * len(intercept_qs) * len(sigma_qs)

def test_compute_likelihood(sample_data):
    """Test computing the likelihood of the data."""
    snow = sample_data.groupby(sample_data["DATE"].dt.year)["SNOW"].sum().reset_index()
    xs = snow["YEAR"] - snow["YEAR"].mean().round()
    ys = snow["SNOW"]
    prior_slope, prior_intercept, prior_sigma = create_priors()
    prior = make_joint3(prior_slope, prior_intercept, prior_sigma)
    likelihood = compute_likelihood(xs, ys, prior)
    assert isinstance(likelihood, Pmf)
    assert len(likelihood) == len(prior)

def test_update_posterior(sample_data):
    """Test updating the posterior."""
    snow = sample_data.groupby(sample_data["DATE"].dt.year)["SNOW"].sum().reset_index()
    xs = snow["YEAR"] - snow["YEAR"].mean().round()
    ys = snow["SNOW"]
    prior_slope, prior_intercept, prior_sigma = create_priors()
    prior = make_joint3(prior_slope, prior_intercept, prior_sigma)
    likelihood = compute_likelihood(xs, ys, prior)
    posterior = update_posterior(prior, likelihood)
    assert isinstance(posterior, Pmf)
    assert np.isclose(posterior.sum(), 1)
