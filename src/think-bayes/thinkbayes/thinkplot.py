"""This file contains code for use with "Think Stats",
by Allen B. Downey, available from greenteapress.com

Copyright 2014 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""
import logging
import math
import warnings

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas
import thinkbayes
from chart_studio import plotly


# customize some matplotlib attributes
# matplotlib.rc('figure', figsize=(4, 3))

# matplotlib.rc('font', size=14.0)
# matplotlib.rc('axes', labelsize=22.0, titlesize=22.0)
# matplotlib.rc('legend', fontsize=20.0)

# matplotlib.rc('xtick.major', size=6.0)
# matplotlib.rc('xtick.minor', size=3.0)

# matplotlib.rc('ytick.major', size=6.0)
# matplotlib.rc('ytick.minor', size=3.0)
POSTERIOR_MARGINAL_LABEL = "Posterior marginal distribution"

class _Brewer(object):
    """Encapsulates a nice sequence of colors.

    Shades of blue that look good in color and can be distinguished
    in grayscale (up to a point).

    Borrowed from https://colorbrewer2.org/
    """

    color_iter = None

    colors_list = [
                      "#f7fbff",
                      "#deebf7",
                      "#c6dbef",
                      "#9ecae1",
                      "#6baed6",
                      "#4292c6",
                      "#2171b5",
                      "#08519c",
                      "#08306b",
                  ][::-1]

    # lists that indicate which colors to use depending on how many are used
    which_colors = [
        [],
        [1],
        [1, 3],
        [0, 2, 4],
        [0, 2, 4, 6],
        [0, 2, 3, 5, 6],
        [0, 2, 3, 4, 5, 6],
        [0, 1, 2, 3, 4, 5, 6],
        [0, 1, 2, 3, 4, 5, 6, 7],
        [0, 1, 2, 3, 4, 5, 6, 7, 8],
    ]

    current_figure = None

    @classmethod
    def colors(cls):
        """Returns the list of colors.
        """
        return cls.colors_list

    @classmethod
    def color_generator(cls, num):
        """Returns an iterator of color strings.

        n: how many colors will be used
        """
        for i in cls.which_colors[num]:
            yield cls.colors_list[i]
        raise StopIteration("Ran out of colors in _Brewer.")

    @classmethod
    def init_iter(cls, num):
        """Initializes the color iterator with the given number of colors."""
        cls.color_iter = cls.color_generator(num)
        fig = plt.gcf()
        cls.current_figure = fig

    @classmethod
    def clear_iter(cls):
        """Sets the color iterator to None."""
        cls.color_iter = None
        cls.current_figure = None

    @classmethod
    def get_iter(cls, num):
        """Gets the color iterator."""
        fig = plt.gcf()
        if fig != cls.current_figure:
            cls.init_iter(num)
            cls.current_figure = fig

        if cls.color_iter is None:
            cls.init_iter(num)

        return cls.color_iter


def _underride_color(options):
    """If color is not in the options, chooses a color.
    """
    if "color" in options:
        return options

    # get the current color iterator; if there is none, init one
    color_iter = _Brewer.get_iter(5)

    try:
        options["color"] = next(color_iter)
    except (StopIteration, RuntimeError):
        # if you run out of colors, initialize the color iterator
        # and try again
        warnings.warn("Ran out of colors.  Starting over.")
        _Brewer.clear_iter()
        _underride_color(options)

    return options


def pre_plot(num=None, rows=None, cols=None):
    """Takes hints about what's coming.

    num: number of lines that will be plotted
    rows: number of rows of subplots
    cols: number of columns of subplots
    """
    if num:
        _Brewer.init_iter(num)

    if rows is None and cols is None:
        return

    if rows is not None and cols is None:
        cols = 1

    if cols is not None and rows is None:
        rows = 1

    # resize the image, depending on the number of rows and cols
    size_map = {
        (1, 1): (8, 6),
        (1, 2): (12, 6),
        (1, 3): (12, 6),
        (1, 4): (12, 5),
        (1, 5): (12, 4),
        (2, 2): (10, 10),
        (2, 3): (16, 10),
        (3, 1): (8, 10),
        (4, 1): (8, 12),
    }

    if (rows, cols) in size_map:
        fig = plt.gcf()
        fig.set_size_inches(*size_map[rows, cols])

    # create the first subplot
    if rows > 1 or cols > 1:
        ax = plt.subplot(rows, cols, 1)
        global SUBPLOT_ROWS, SUBPLOT_COLS
        SUBPLOT_ROWS = rows
        SUBPLOT_COLS = cols
    else:
        ax = plt.gca()

    return ax


def sub_plot(plot_number, rows=None, cols=None, **options):
    """Configures the number of subplots and changes the current plot.

    rows: int
    cols: int
    plot_number: int
    options: passed to subplot
    """
    rows = rows or SUBPLOT_ROWS
    cols = cols or SUBPLOT_COLS
    return plt.subplot(rows, cols, plot_number, **options)


def _underride(d, **options):
    """Add key-value pairs to d only if key is not in d.

    If d is None, create a new dictionary.

    d: dictionary
    options: keyword args to add to d
    """
    if d is None:
        d = {}

    for key, val in options.items():
        d.setdefault(key, val)

    return d


def clear_figure():
    """Clears the figure and any hints that have been set."""
    global LOC
    LOC = None
    _Brewer.clear_iter()
    plt.clf()
    fig = plt.gcf()
    fig.set_size_inches(8, 6)


def underride_figure(**options):
    """Sets options for the current figure."""
    _underride(options, figsize=(6, 8))
    plt.figure(**options)


def plot_line(obj, ys=None, style="", **options):
    """Plots a line.

    Args:
      obj: sequence of x values, or Series, or anything with Render()
      ys: sequence of y values
      style: style string passed along to plt.plot
      options: keyword args passed to plt.plot
    """

    label = getattr(obj, "label", "_nolegend_")
    options = _underride_color(options)
    options = _underride(options, linewidth=3, alpha=0.7, label=label)

    xs = obj
    if ys is None:
        if hasattr(obj, "Render"):
            xs, ys = obj.render()
        if isinstance(obj, pandas.Series):
            ys = obj.values
            xs = obj.index

    if ys is None:
        plt.plot(xs, style, **options)
    else:
        plt.plot(xs, ys, style, **options)


def plot_vlines(xs, y1, y2, **options):
    """Plots a set of vertical lines.

    Args:
      xs: sequence of x values
      y1: sequence of y values
      y2: sequence of y values
      options: keyword args passed to plt.vlines
    """
    options = _underride_color(options)
    options = _underride(options, linewidth=1, alpha=0.5)
    plt.vlines(xs, y1, y2, **options)


def plot_hlines(ys, x1, x2, **options):
    """Plots a set of horizontal lines.

    Args:
      ys: sequence of y values
      x1: sequence of x values
      x2: sequence of x values
      options: keyword args passed to plt.vlines
    """
    options = _underride_color(options)
    options = _underride(options, linewidth=1, alpha=0.5)
    plt.hlines(ys, x1, x2, **options)


def fill_between_lines(xs, y1, y2=None, where=None, **options):
    """Fills the space between two lines.

    Args:
      xs: sequence of x values
      y1: sequence of y values
      y2: sequence of y values
      where: sequence of boolean
      options: keyword args passed to plt.fill_between
    """
    options = _underride_color(options)
    options = _underride(options, linewidth=0, alpha=0.5)
    plt.fill_between(xs, y1, y2, where, **options)


def bar_plot(xs, ys, **options):
    """Plots a line.

    Args:
      xs: sequence of x values
      ys: sequence of y values
      options: keyword args passed to plt.bar
    """
    options = _underride_color(options)
    options = _underride(options, linewidth=0, alpha=0.6)
    plt.bar(xs, ys, **options)


def scatter_plot(xs, ys=None, **options):
    """Makes a scatter plot.

    xs: x values
    ys: y values
    options: options passed to plt.scatter
    """
    options = _underride(options, color="blue", alpha=0.2, s=30, edgecolors="none")

    if ys is None and isinstance(xs, pandas.Series):
        ys = xs.values
        xs = xs.index

    plt.scatter(xs, ys, **options)


def scatter_hexbin(xs, ys, **options):
    """Makes a scatter plot.

    xs: x values
    ys: y values
    options: options passed to plt.scatter
    """
    options = _underride(options, cmap=matplotlib.cm.Blues)
    plt.hexbin(xs, ys, **options)


def plot_pdf_line(pdf, **options):
    """Plots a Pdf, Pmf, or Hist as a line.

    Args:
      pdf: Pdf, Pmf, or Hist object
      options: keyword args passed to plt.plot
    """
    low, high = options.pop("low", None), options.pop("high", None)
    logging.debug("%r", f"low={low}")
    logging.debug("%r", f"high={high}")

    n = options.pop("n", 101)
    logging.debug("%r", f"n={n}")

    xs, ps = pdf.render()
    options = _underride(options, label=pdf.label)
    plot_line(xs, ps, **options)


def plot_pdfs(pdfs, **options):
    """Plots a sequence of PDFs.

    Options are passed along for all PDFs.  If you want different
    options for each pdf, make multiple calls to Pdf.

    Args:
      pdfs: sequence of PDF objects
      options: keyword args passed to plt.plot
    """
    for pdf in pdfs:
        plot_pdf_line(pdf, **options)


def plot_hist_bar(histogram, **options):
    """Plots a Pmf or Hist with a bar plot.

    The default width of the bars is based on the minimum difference
    between values in the Hist.  If that's too small, you can override
    it by providing a width keyword argument, in the same units
    as the values.

    Args:
      histogram: Hist or Pmf object
      options: keyword args passed to plt.bar
    """
    # find the minimum distance between adjacent values
    xs, ys = histogram.render()

    # see if the values support arithmetic
    try:
        assert "__sub__" in dir(xs)
    except AssertionError:
        # if not, replace values with numbers
        labels = [str(x) for x in xs]
        xs = np.arange(len(xs))
        plt.xticks(xs + 0.5, labels)

    if "width" not in options:
        try:
            options["width"] = 0.9 * np.nanmin(np.diff(xs))
        except TypeError:
            warnings.warn(
                "Hist: Can't compute bar width automatically."
                "Check for non-numeric types in Hist."
                "Or try providing width option."
            )

    options = _underride(options, label=histogram.label)
    options = _underride(options, align="center")
    if options["align"] == "left":
        options["align"] = "edge"
    elif options["align"] == "right":
        options["align"] = "edge"
        options["width"] *= -1

    bar_plot(xs, ys, **options)


def plot_interleaved_hists(histograms, **options):
    """Plots two histograms as interleaved bar plots.

    Options are passed along for all PMFs.  If you want different
    options for each pmf, make multiple calls to Pmf.

    Args:
      histograms: list of two Hist or Pmf objects
      options: keyword args passed to plt.plot
    """
    for histogram in histograms:
        plot_hist_bar(histogram, **options)


def plot_pmf_line(probability_mass_function, **options):
    """Plots a Pmf or Hist as a line.

    Args:
      probability_mass_function: Hist or Pmf object
      options: keyword args passed to plt.plot
    """
    xs, ys = probability_mass_function.render()
    low, high = min(xs), max(xs)
    logging.debug("%r", f"low={low}")
    logging.debug("%r", f"high={high}")

    width = options.pop("width", None)
    if width is None:
        try:
            width = np.nanmin(np.diff(xs))
        except TypeError:
            warnings.warn(
                "Pmf: Can't compute bar width automatically."
                "Check for non-numeric types in Pmf."
                "Or try providing width option."
            )
    points = []

    lastx = np.nan
    lasty = 0
    for x, y in zip(xs, ys):
        if (x - lastx) > 1e-5:
            points.append((lastx, 0))
            points.append((x, 0))

        points.append((x, lasty))
        points.append((x, y))
        points.append((x + width, y))

        lastx = x + width
        lasty = y
    points.append((lastx, 0))
    pxs, pys = zip(*points)

    align = options.pop("align", "center")
    if align == "center":
        pxs = np.array(pxs) - width / 2.0
    if align == "right":
        pxs = np.array(pxs) - width

    options = _underride(options, label=probability_mass_function.label)
    plot_line(pxs, pys, **options)


def plot_pmfs(probability_mass_functions, **options):
    """Plots a sequence of PMFs.

    Options are passed along for all PMFs.  If you want different
    options for each pmf, make multiple calls to Pmf.

    Args:
      probability_mass_functions: sequence of PMF objects
      options: keyword args passed to plt.plot
    """
    for probability_mass_function in probability_mass_functions:
        plot_pmf_line(probability_mass_function, **options)


def differences(t):
    """Compute the differences between adjacent elements in a sequence.

    Args:
        t: sequence of number

    Returns:
        sequence of differences (length one less than t)
    """
    diffs = [t[i + 1] - t[i] for i in range(len(t) - 1)]
    return diffs


def plot_cdf_line(cumulative_density_function, complement=False, transform=None, **options):
    """Plots a CDF as a line.

    Args:
      cumulative_density_function: Cdf object
      complement: boolean, whether to plot the complementary CDF
      transform: string, one of 'exponential', 'pareto', 'weibull', 'gumbel'
      options: keyword args passed to plt.plot

    Returns:
      dictionary with the scale options that should be passed to
      Config, Show or Save.
    """
    xs, ps = cumulative_density_function.render()
    xs = np.asarray(xs)
    ps = np.asarray(ps)

    scale = dict(xscale="linear", yscale="linear")

    for s in ["xscale", "yscale"]:
        if s in options:
            scale[s] = options.pop(s)

    if transform == "exponential":
        complement = True
        scale["yscale"] = "log"

    if transform == "pareto":
        complement = True
        scale["yscale"] = "log"
        scale["xscale"] = "log"

    if complement:
        ps = [1.0 - p for p in ps]

    if transform == "weibull":
        xs = np.delete(xs, -1)
        ps = np.delete(ps, -1)
        ps = [-math.log(1.0 - p) for p in ps]
        scale["xscale"] = "log"
        scale["yscale"] = "log"

    if transform == "gumbel":
        xs = np.delete(xs, 0)
        ps = np.delete(ps, 0)
        ps = [-math.log(p) for p in ps]
        scale["yscale"] = "log"

    options = _underride(options, label=cumulative_density_function.label)
    plot_line(xs, ps, **options)
    return scale


def plot_cdfs(cumulative_density_functions, complement=False, transform=None, **options):
    """Plots a sequence of CDFs.

    cdfs: sequence of CDF objects
    complement: boolean, whether to plot the complementary CDF
    transform: string, one of 'exponential', 'pareto', 'weibull', 'gumbel'
    options: keyword args passed to plt.plot
    """
    for cumulative_density_function in cumulative_density_functions:
        plot_cdf_line(cumulative_density_function, complement, transform, **options)


def plot_cdf(cumulative_density_function):
    """
    Draws a Cdf with vertical lines at the observed test stat.
    """

    def vert_line(x):
        """Draws a vertical line at x."""
        plt.plot([x, x], [0, 1], color="0.8")

    vert_line(cumulative_density_function.actual)
    plot_cdf_line(cumulative_density_function, complement=False)


def contour_plot(obj, pcolor_bool=False, contour_bool=True, imshow=False, **options):
    """Makes a contour plot.

    d: map from (x, y) to z, or object that provides GetDict
    pcolor: boolean, whether to make a pseudocolor plot
    contour: boolean, whether to make a contour plot
    imshow: boolean, whether to use plt.imshow
    options: keyword args passed to plt.pcolor and/or plt.contour
    """
    try:
        d = obj.d
    except AttributeError:
        d = obj

    _underride(options, cmap=matplotlib.cm.Blues)

    xs, ys = zip(*d.keys())
    xs = sorted(set(xs))
    ys = sorted(set(ys))

    x_meshgrid, y_meshgrid = np.meshgrid(xs, ys)

    def func(x, y):
        return d.get((x, y), 0)

    func = np.vectorize(func)
    z_meshgrid = func(x_meshgrid, y_meshgrid)

    x_formatter = matplotlib.ticker.ScalarFormatter(useOffset=False)
    axes = plt.gca()
    axes.xaxis.set_major_formatter(x_formatter)

    if pcolor_bool:
        plt.pcolormesh(x_meshgrid, y_meshgrid, z_meshgrid, **options)
    if contour_bool:
        cs = plt.contour(x_meshgrid, y_meshgrid, z_meshgrid, **options)
        plt.clabel(cs, inline=1, fontsize=10)
    if imshow:
        extent = xs[0], xs[-1], ys[0], ys[-1]
        plt.imshow(z_meshgrid, extent=extent, **options)


def pseudocolor(xs, ys, zs, pcolor_bool=True, contour_bool=False, **options):
    """Makes a pseudocolor plot.

    xs:
    ys:
    zs:
    pcolor: boolean, whether to make a pseudocolor plot
    contour: boolean, whether to make a contour plot
    options: keyword args passed to plt.pcolor and/or plt.contour
    """
    _underride(options, linewidth=3, cmap=matplotlib.cm.Blues)

    x_meshgrid, y_meshgrid = np.meshgrid(xs, ys)
    z_meshgrid = zs

    x_formatter = matplotlib.ticker.ScalarFormatter(useOffset=False)
    axes = plt.gca()
    axes.xaxis.set_major_formatter(x_formatter)

    if pcolor_bool:
        plt.pcolormesh(x_meshgrid, y_meshgrid, z_meshgrid, **options)

    if contour_bool:
        cs = plt.contour(x_meshgrid, y_meshgrid, z_meshgrid, **options)
        plt.clabel(cs, inline=1, fontsize=10)


def annotate_figure(x, y, s, **options):
    """Puts text in a figure.

    x: number
    y: number
    s: string
    options: keyword args passed to plt.text
    """
    options = _underride(
        options, fontsize=16, verticalalignment="top", horizontalalignment="left"
    )
    plt.text(x, y, s, **options)


LEGEND = True
LOC = None


def config_plot(**options):
    """Configures the plot.

    Pulls options out of the option dictionary and passes them to
    the corresponding plt functions.
    """
    names = [
        "title",
        "xlabel",
        "ylabel",
        "xscale",
        "yscale",
        "xticks",
        "yticks",
        "axis",
        "xlim",
        "ylim",
    ]

    for name in names:
        if name in options:
            getattr(plt, name)(options[name])

    global LEGEND
    LEGEND = options.get("legend", LEGEND)

    if LEGEND:
        global LOC
        LOC = options.get("loc", LOC)
        frameon = options.get("frameon", True)

        warnings.filterwarnings("error", category=UserWarning)
        try:
            plt.legend(loc=LOC, frameon=frameon)
        except UserWarning:
            pass
        warnings.filterwarnings("default", category=UserWarning)

    # x and y ticklabels can be made invisible
    val = options.get("xticklabels", None)
    if val is not None:
        if val == "invisible":
            ax = plt.gca()
            labels = ax.get_xticklabels()
            plt.setp(labels, visible=False)

    val = options.get("yticklabels", None)
    if val is not None:
        if val == "invisible":
            ax = plt.gca()
            labels = ax.get_yticklabels()
            plt.setp(labels, visible=False)


def decorate(**options):
    """Decorate the current axes.

    Call decorate with keyword arguments like

    decorate(title='Title',
             xlabel='x',
             ylabel='y')

    The keyword arguments can be any of the axis properties

    https://matplotlib.org/api/axes_api.html

    In addition, you can use `legend=False` to suppress the legend.

    And you can use `loc` to indicate the location of the legend
    (the default value is 'best')
    """
    loc = options.pop("loc", "best")
    if options.pop("legend", True):
        legend(loc=loc)

    plt.gca().set(**options)
    plt.tight_layout()


def legend(**options):
    """Draws a legend only if there is at least one labeled item.

    options are passed to plt.legend()
    https://matplotlib.org/api/_as_gen/matplotlib.pyplot.legend.html

    """
    _underride(options, loc="best")

    ax = plt.gca()
    handles, labels = ax.get_legend_handles_labels()
    if len(handles):
        ax.legend(handles, labels, **options)


def show_plot(**options):
    """Shows the plot.

    For options, see Config.

    options: keyword args used to invoke various plt functions
    """
    clf_bool = options.pop("clf", True)
    config_plot(**options)
    plt.show()
    if clf_bool:
        clear_figure()


def show_plotly(**options):
    """Shows the plot.

    For options, see Config.

    options: keyword args used to invoke various plt functions
    """
    clf_bool = options.pop("clf", True)
    config_plot(**options)

    url = plotly.plot_mpl(plt.gcf())
    if clf_bool:
        clear_figure()
    return url


def save_plot(root=None, formats=None, **options):
    """Saves the plot in the given formats and clears the figure.

    For options, see Config.

    Args:
      root: string filename root
      formats: list of string formats
      options: keyword args used to invoke various plt functions
    """
    clf_bool = options.pop("clf", True)

    save_options = {}
    for option in ["bbox_inches", "pad_inches"]:
        if option in options:
            save_options[option] = options.pop(option)

    config_plot(**options)

    if formats is None:
        formats = ["pdf", "png"]

    try:
        formats.remove("plotly")
        show_plotly(clf=False)
    except ValueError:
        pass

    if root:
        for fmt in formats:
            save_format(root, fmt, **save_options)
    if clf_bool:
        clear_figure()


def save_format(root, fmt="eps", **options):
    """Writes the current figure to a file in the given format.

    Args:
      root: string filename root
      fmt: string format
    """
    _underride(options, dpi=300)
    filename = "%s.%s" % (root, fmt)
    print("Writing", filename)
    plt.savefig(filename, format=fmt, **options)


def render_pdf(mu, sigma, n=101):
    """Makes xs and ys for a normal PDF with (mu, sigma).

    n: number of places to evaluate the PDF
    """
    xs = np.linspace(mu - 4 * sigma, mu + 4 * sigma, n)
    ys = [thinkbayes.eval_normal_pdf(x, mu, sigma) for x in xs]
    return xs, ys


# provide aliases for calling functions with lower-case names
preplot = pre_plot
subplot = sub_plot
clf = clear_figure
figure = underride_figure
plot = plot_line
vlines = plot_vlines
hlines = plot_hlines
fill_between = fill_between_lines
text = annotate_figure
scatter = scatter_plot
pmf = plot_pmf_line
pmfs = plot_pmfs
hist = plot_hist_bar
hists = plot_interleaved_hists
diff = differences
cdf = plot_cdf_line
cdfs = plot_cdfs
contour = contour_plot
pcolor = pseudocolor
config = config_plot
show = show_plot
save = save_plot


def main():
    color_iter = _Brewer.color_generator(7)
    for color in color_iter:
        print(color)


if __name__ == "__main__":
    main()
