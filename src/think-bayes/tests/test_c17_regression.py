import numpy as np
import pandas as pd
from empiricaldist import Pmf, Cdf
from matplotlib import pyplot as plt
from scipy.stats import norm

from thinkbayes.c17_regression import (
    update_posterior,
    load_data,
    prepare_data,
    fit_regression, RegressionModel, make_uniform, make_joint3, compute_likelihood
)


def test_load_data():
    df = load_data("/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv")
    assert df.shape == (19360, 20)


def test_prepare_data(tmp_path):
    """Test loading and preparing data."""
    df = pd.DataFrame({
        "DATE": pd.date_range(start="1967-01-01", periods=5, freq="Y"),
        "SNOW": [10, 20, 30, 25, 35],
    })
    df = prepare_data(df)
    assert df.shape == (5,)


def test_prepare_data_load_data():
    df = prepare_data(load_data("/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv"))
    assert df.shape == (54,)


def test_create_pmf():
    """
    Test creating a PMF, compare with a theoretical distribution.
    """
    # Load and prepare data
    file_path = "/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv"
    df = prepare_data(load_data(file_path))

    # Calculate observed statistics

    mean_obs = df.mean()
    std_obs = df.std()

    # Create PMF and CDF from data
    pmf_obs = Pmf.from_seq(df)

    # Validate PMF properties
    mean_pmf = pmf_obs.mean()
    std_pmf = pmf_obs.std()

    assert isinstance(pmf_obs, Pmf), "pmf_obs is not an instance of Pmf"
    assert np.isclose(mean_pmf, mean_obs), "Mean from PMF does not match observed mean"
    assert np.isclose(std_pmf, std_obs, atol=2), "Std from PMF does not match observed std"


def test_create_cdf_plot():
    """
    Test creating a PMF and CDF from snowfall data and compare with a theoretical distribution.
    The y-axis is denormalized to show count frequencies.
    """
    # Load and prepare data
    file_path = "/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv"
    df = prepare_data(load_data(file_path))

    # Calculate observed statistics
    total_count = len(df)  # Total number of data points
    mean_obs = df.mean()
    std_obs = df.std()

    # Create PMF and CDF from data
    pmf_obs = Pmf.from_seq(df)
    cdf_obs = Cdf.from_seq(df)

    # Create a theoretical normal distribution
    theoretical_dist = norm(mean_obs, std_obs)
    theoretical_cdf = theoretical_dist.cdf(pmf_obs.qs)

    # Denormalize the CDF values to represent count frequencies
    observed_cdf_counts = cdf_obs.ps * total_count
    theoretical_cdf_counts = theoretical_cdf * total_count

    # Plot observed and theoretical CDFs
    fig, ax = plt.subplots(figsize=(8, 5))
    ax.step(cdf_obs.qs, observed_cdf_counts, where="post", label="Observed CDF (Counts)")
    ax.plot(pmf_obs.qs, theoretical_cdf_counts, color="C5", label="Theoretical CDF (Counts)", linestyle="--")

    # Customize plot
    ax.set_title("Observed vs Theoretical CDF (Count Frequency)")
    ax.set_xlabel("Snowfall (units)")
    ax.set_ylabel("Cumulative Count")
    ax.legend()
    ax.yaxis.grid(True, which="major", linestyle="--", color="gray", alpha=0.7)  # Grid for y-axis only
    ax.xaxis.grid(False)  # No gridlines for x-axis

    # Show and close plot
    plt.tight_layout()
    plt.show()
    plt.close()


def test_fit_regression():
    """Test fitting a regression model."""
    df = prepare_data(load_data("/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv"))
    results = fit_regression(data=df)
    assert isinstance(results.offset, float)
    assert results == RegressionModel(
        intercept=np.float64(62.78048916841369),
        offset=np.float64(1994.0),
        sigma=np.float64(26.566114647742996),
        slope=np.float64(0.42394129979035633)
    )


def test_make_uniform():
    """Test creating a uniform prior."""
    qs = np.linspace(-0.5, 1.5, 51)
    prior = make_uniform(qs, "Slope")
    assert isinstance(prior, Pmf)
    assert np.isclose(prior.sum(), 1)
    assert len(prior) == len(qs)


def test_create_priors():
    """Test creating priors for slope, intercept, and sigma."""
    # Create priors
    prior_slope = make_uniform(np.linspace(-0.5, 1.5, 51))
    assert len(prior_slope) == 51
    prior_intercept = make_uniform(np.linspace(54, 75, 41))
    assert len(prior_intercept) == 41
    prior_sigma = make_uniform(np.linspace(20, 35, 31))
    assert len(prior_sigma) == 31
    prior = make_joint3(prior_slope, prior_intercept, prior_sigma)
    print(f"prior = {prior}")
    assert len(prior) == 51 * 41 * 31


def test_make_joint3():
    """Test creating a joint distribution."""
    slope_qs = np.linspace(-0.5, 1.5, 51)
    intercept_qs = np.linspace(54, 75, 41)
    sigma_qs = np.linspace(20, 35, 31)
    prior_slope = make_uniform(slope_qs)
    prior_intercept = make_uniform(intercept_qs)
    prior_sigma = make_uniform(sigma_qs)
    joint = make_joint3(prior_slope, prior_intercept, prior_sigma)
    assert isinstance(joint, Pmf)
    assert len(joint) == len(slope_qs) * len(intercept_qs) * len(sigma_qs)


def test_compute_likelihood():
    # Load and prepare data
    df = prepare_data(load_data("/mnt/SSD1/mrepos/github.com/wilsonify/ThinkBayes2/data/2239075.csv"))
    model = fit_regression(df)
    print(f"model = {model}")

    # Create priors
    slope_qs = np.linspace(-0.5, 1.5, 50)
    prior_slope = make_uniform(slope_qs)

    intercept_qs = np.linspace(54, 75, 50)
    prior_intercept = make_uniform(intercept_qs)

    sigma_qs = np.linspace(20, 35, 50)
    prior_sigma = make_uniform(sigma_qs)

    prior = make_joint3(prior_slope, prior_intercept, prior_sigma)
    print(f"prior = {prior}")

    likelihood = compute_likelihood(df, model)
    assert likelihood == 0.0


def test_update_posterior():
    """Test updating the posterior."""
    sample_data = pd.DataFrame({
        "DATE": pd.date_range(start="1967-01-01", periods=5, freq="Y"),
        "SNOW": [10, 20, 30, 25, 35],
    })
    sample_data["YEAR"] = sample_data["DATE"].dt.year
    snow = sample_data.groupby(sample_data["YEAR"])["SNOW"].sum().reset_index()
    xs = snow["YEAR"] - snow["YEAR"].mean().round()
    ys = snow["SNOW"]
    prior_slope, prior_intercept, prior_sigma = create_priors()
    prior = make_joint3(prior_slope, prior_intercept, prior_sigma)
    likelihood = compute_likelihood(xs, ys, prior)
    posterior = update_posterior(prior, likelihood)
    assert isinstance(posterior, Pmf)
    assert np.isclose(posterior.sum(), 1)
