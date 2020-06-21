"""This file contains code used in "Think Bayes",
by Allen B. Downey, available from greenteapress.com

Copyright 2012 Allen B. Downey
MIT License: https://opensource.org/licenses/MIT
"""

import csv
import logging
import shelve
import sys
import time
import warnings

import matplotlib.pyplot as pyplot
import numpy as np
import thinkbayes
from thinkbayes import thinkplot

warnings.simplefilter("error", RuntimeWarning)

FORMATS = ["pdf", "eps", "png"]
SPECIES_LABEL = "Number of species"


class Locker(object):
    """Encapsulates a shelf for storing key-value pairs."""

    def __init__(self, shelf_file):
        self.shelf = shelve.open(shelf_file)

    def close(self):
        """Closes the shelf.
        """
        self.shelf.close()

    def add(self, key, value):
        """Adds a key-value pair."""
        self.shelf[str(key)] = value

    def lookup(self, key):
        """Looks up a key."""
        return self.shelf.get(str(key))

    def keys(self):
        """Returns an iterator of keys."""
        return self.shelf.keys()

    def read(self):
        """Returns the contents of the shelf as a map."""
        return dict(self.shelf)


class Subject(object):
    """Represents a subject from the belly button study."""

    def __init__(self, code):
        """
        code: string ID
        species: sequence of (int count, string species) pairs
        """
        self.code = code
        self.species = []
        self.suite = None
        self.num_reads = None
        self.num_species = None
        self.total_reads = None
        self.total_species = None
        self.prev_unseen = None
        self.pmf_n = None
        self.pmf_q = None
        self.pmf_l = None

    def add_species_count(self, species, count):
        """Add a species-count pair.

        It is up to the caller to ensure that species names are unique.

        species: string species/genus name
        count: int number of individuals
        """
        self.species.append((count, species))

    def done(self, reverse=False, clean_param=0):
        """Called when we are done adding species counts.

        reverse: which order to sort in
        """
        if clean_param:
            self.clean(clean_param)

        self.species.sort(reverse=reverse)
        counts = self.get_counts()
        self.num_species = len(counts)
        self.num_reads = sum(counts)

    def clean(self, clean_param=50):
        """Identifies and removes bogus data.

        clean_param: parameter that controls the number of legit species
        """

        def prob_bogus(k_species, r_species):
            """Compute the probability that a species is bogus."""
            q = clean_param / r_species
            p = (1 - q) ** k_species
            return p

        print(self.code, clean_param)

        counts = self.get_counts()
        r = 1.0 * sum(counts)

        species_seq = []
        for k, species in sorted(self.species):

            if np.random.random() < prob_bogus(k, r):
                continue
            species_seq.append((k, species))
        self.species = species_seq

    def get_m(self):
        """Gets number of observed species."""
        return len(self.species)

    def get_counts(self):
        """Gets the list of species counts

        Should be in increasing order, if Sort() has been invoked.
        """
        return [count for count, _ in self.species]

    def make_cdf(self):
        """Makes a CDF of total prevalence vs rank."""
        counts = self.get_counts()
        counts.sort(reverse=True)
        cdf = thinkbayes.Cdf(dict(enumerate(counts)))
        return cdf

    def get_names(self):
        """Gets the names of the seen species."""
        return [name for _, name in self.species]

    def print_counts(self):
        """Prints the counts and species names."""
        for count, name in reversed(self.species):
            print(count, name)

    def get_cdf(self):
        """Returns cumulative prevalence vs number of species.
        """
        counts = self.get_counts()
        items = enumerate(counts)
        cdf = thinkbayes.Cdf(items)
        return cdf

    def get_prevalences(self):
        """Returns a sequence of prevalences (normalized counts).
        """
        counts = self.get_counts()
        total = sum(counts)
        prevalences = np.array(counts, dtype=np.float) / total
        return prevalences

    def process(self, low=None, high=500, conc=1.0, iters=100):
        """Computes the posterior distribution of n and the prevalences.

        Sets attribute: self.suite

        low: minimum number of species
        high: maximum number of species
        conc: concentration parameter
        iters: number of iterations to use in the estimator
        """
        counts = self.get_counts()
        m = len(counts)
        if low is None:
            low = max(m, 2)
        ns = range(low, high + 1)

        # start = time.time()
        self.suite = Species5(ns, conc=conc, iters=iters)
        self.suite.update(counts)
        # end = time.time()

        # print 'Processing time' end-start

    def make_prediction(self, num_sims=100):
        """Make predictions for the given subject.

        Precondition: Process has run

        num_sims: how many simulations to run for predictions

        Adds attributes
        pmf_l: predictive distribution of additional species
        """
        add_reads = self.total_reads - self.num_reads
        curves = self.run_simulations(num_sims, add_reads)
        self.pmf_l = self.make_predictive(curves)

    def make_quick_prediction(self, num_sims=100):
        """Make predictions for the given subject.

        Precondition: Process has run

        num_sims: how many simulations to run for predictions

        Adds attribute:
        pmf_l: predictive distribution of additional species
        """
        add_reads = self.total_reads - self.num_reads
        pmf = thinkbayes.Pmf()
        _, seen = self.get_seen_species()

        for _ in range(num_sims):
            _, observations = self.generate_observations(add_reads)
            all_seen = seen.union(observations)
            len_all_seen_minus_seen = len(all_seen) - len(seen)
            pmf.incr(len_all_seen_minus_seen)

        pmf.normalize()
        self.pmf_l = pmf

    def dist_l(self):
        """Returns the distribution of additional species, l.
        """
        return self.pmf_l

    def make_figures(self):
        """Makes figures showing distribution of n and the prevalences."""
        self.plot_dist_n()
        self.plot_prevalences()

    def plot_dist_n(self):
        """Plots distribution of n."""
        pmf = self.suite.dist_n()
        print("90% CI for N:", pmf.credible_interval(90))
        pmf.label = self.code

        thinkplot.clear_figure()
        thinkplot.pre_plot(num=1)

        thinkplot.plot_pmf_line(pmf)

        root = "species-ndist-%s" % self.code

        thinkplot.save_plot(
            root=root, xlabel=SPECIES_LABEL, ylabel="Prob", formats=FORMATS,
        )

    def plot_prevalences(self, num=5):
        """Plots dist of prevalence for several species.

        num: how many species (starting with the highest prevalence)
        """
        thinkplot.clear_figure()
        thinkplot.pre_plot(num=5)

        for rank in range(1, num + 1):
            self.plot_prevalence(rank)

        root = "species-prev-%s" % self.code
        thinkplot.save_plot(
            root=root,
            xlabel="Prevalence",
            ylabel="Prob",
            formats=FORMATS,
            axis=[0, 0.3, 0, 1],
        )

    def plot_prevalence(self, rank=1, cdf_flag=True):
        """Plots dist of prevalence for one species.

        rank: rank order of the species to plot.
        cdf_flag: whether to plot the CDF
        """
        # convert rank to index
        index = self.get_m() - rank

        _, mix = self.suite.dist_of_prevalence(index)
        count, _ = self.species[index]
        mix.label = f"{rank} ({count})"

        print(f"90%% CI for prevalence of species {rank}: ")
        print(mix.credible_interval(90))

        if cdf_flag:
            cdf = mix.make_cdf()
            thinkplot.plot_cdf_line(cdf)
        else:
            thinkplot.plot_pmf_line(mix)

    def plot_mixture(self, rank=1):
        """Plots dist of prevalence for all n, and the mix.

        rank: rank order of the species to plot
        """
        # convert rank to index
        index = self.get_m() - rank

        print(self.species[index])
        print(self.get_counts()[index])

        metapmf, mix = self.suite.dist_of_prevalence(index)

        thinkplot.clear_figure()
        for pmf in metapmf.values():
            thinkplot.plot_pmf_line(pmf, color="blue", alpha=0.2, linewidth=0.5)

        thinkplot.plot_pmf_line(mix, color="blue", alpha=0.9, linewidth=2)

        root = "species-mix-%s" % self.code
        thinkplot.save_plot(
            root=root,
            xlabel="Prevalence",
            ylabel="Prob",
            formats=FORMATS,
            axis=[0, 0.3, 0, 0.3],
            legend=False,
        )

    def get_seen_species(self):
        """Makes a set of the names of seen species.

        Returns: number of species, set of string species names
        """
        names = self.get_names()
        m = len(names)
        seen = set(species_generator(names, m))
        return m, seen

    def generate_observations(self, num_reads):
        """Generates a series of random observations.

        num_reads: number of reads to generate

        Returns: number of species, sequence of string species names
        """
        n, prevalences = self.suite.sample_posterior()

        names = self.get_names()
        name_iter = species_generator(names, n)

        items = zip(name_iter, prevalences)

        cdf = thinkbayes.Cdf(dict(items))
        observations = cdf.sample(num_reads)

        # for ob in observations:
        #    print ob

        return n, observations

    def resample(self, num_reads):
        """Choose a random subset of the data (without replacement).

        num_reads: number of reads in the subset
        """
        t = []
        for count, species in self.species:
            t.extend([species] * count)

        np.random.shuffle(t)
        reads = t[:num_reads]

        subject = Subject(self.code)
        hist = thinkbayes.Hist(reads)
        for species, count in hist.items():
            subject.add_species_count(species, count)

        subject.done()
        return subject

    def match(self, match):
        """Match up a rarefied subject with a complete subject.

        match: complete Subject

        Assigns attributes:
        total_reads:
        total_species:
        prev_unseen:
        """
        self.total_reads = match.num_reads
        self.total_species = match.num_species

        # compute the prevalence of unseen species (at least approximately,
        # based on all species counts in match
        _, seen = self.get_seen_species()

        seen_total = 0.0
        unseen_total = 0.0
        for count, species in match.species:
            if species in seen:
                seen_total += count
            else:
                unseen_total += count

        self.prev_unseen = unseen_total / (seen_total + unseen_total)

    def run_simulation(self, num_reads, frac_flag=False, jitter=0.01):
        """Simulates additional observations and returns a rarefaction curve.

        k is the number of additional observations
        num_new is the number of new species seen

        num_reads: how many new reads to simulate
        frac_flag: whether to convert to fraction of species seen
        jitter: size of jitter added if frac_flag is true

        Returns: list of (k, num_new) pairs
        """
        m, seen = self.get_seen_species()
        n, observations = self.generate_observations(num_reads)

        curve = []
        for i, obs in enumerate(observations):
            seen.add(obs)

            if frac_flag:
                frac_seen = len(seen) / float(n)
                frac_seen += np.random.uniform(-jitter, jitter)
                curve.append((i + 1, frac_seen))
            else:
                num_new = len(seen) - m
                curve.append((i + 1, num_new))

        return curve

    def run_simulations(self, num_sims, num_reads, frac_flag=False):
        """Runs simulations and returns a list of curves.

        Each curve is a sequence of (k, num_new) pairs.

        num_sims: how many simulations to run
        num_reads: how many samples to generate in each simulation
        frac_flag: whether to convert num_new to fraction of total
        """
        curves = [self.run_simulation(num_reads, frac_flag) for _ in range(num_sims)]
        return curves

    def make_predictive(self, curves):
        """Makes a predictive distribution of additional species.

        curves: list of (k, num_new) curves 

        Returns: Pmf of num_new
        """
        pred = thinkbayes.Pmf(label=self.code)
        for curve in curves:
            _, last_num_new = curve[-1]
            pred.incr(last_num_new)
        pred.normalize()
        return pred


def make_conditionals(curves, ks):
    """Makes Cdfs of the distribution of num_new conditioned on k.

    curves: list of (k, num_new) curves 
    ks: list of values of k

    Returns: list of Cdfs
    """
    joint = make_joint_predictive(curves)

    cdfs = []
    for k in ks:
        pmf = joint.conditional(1, 0, k)
        pmf.label = f"k={k}"
        cdf = pmf.make_cdf()
        cdfs.append(cdf)
        print(f"90%% credible interval for {k} ")
        print(cdf.credible_interval(90))
    return cdfs


def make_joint_predictive(curves):
    """Makes a joint distribution of k and num_new.

    curves: list of (k, num_new) curves 

    Returns: joint Pmf of (k, num_new)
    """
    joint = thinkbayes.Joint()
    for curve in curves:
        for k, num_new in curve:
            joint.incr((k, num_new))
    joint.normalize()
    return joint


def make_frac_cdfs(curves, ks):
    """Makes Cdfs of the fraction of species seen.

    curves: list of (k, num_new) curves 

    Returns: list of Cdfs
    """
    d = {}
    for curve in curves:
        for k, frac in curve:
            if k in ks:
                d.setdefault(k, []).append(frac)

    cdfs = {}
    for k, fracs in d.items():
        cdf = thinkbayes.Cdf(fracs)
        cdfs[k] = cdf

    return cdfs


def species_generator(names, num):
    """Generates a series of names, starting with the given names.

    Additional names are 'unseen' plus a serial number.

    names: list of strings
    num: total number of species names to generate

    Returns: string iterator
    """
    i = 0
    for name in names:
        yield name
        i += 1

    while i < num:
        yield f"unseen-{i}"
        i += 1


def read_rarefacted_data(filename="journal.pone.0047712.s001.csv", clean_param=0):
    """Reads a data file and returns a list of Subjects.

    Data from https://www.plosone.org/article/
    info%3Adoi%2F10.1371%2Fjournal.pone.0047712#s4

    filename: string filename to read
    clean_param: parameter passed to Clean

    Returns: map from code to Subject
    """
    fp = open(filename)
    reader = csv.reader(fp)
    # _ = reader.next()
    _ = next(reader)

    subject = Subject("")
    subject_map = {}

    i = 0
    for t in reader:
        code = t[0]
        if code != subject.code:
            # start a new subject
            subject = Subject(code)
            subject_map[code] = subject

        # append a number to the species names so they're unique
        species = t[1]
        species = f"{species}-{i}"
        i += 1

        count = int(t[2])
        subject.add_species_count(species, count)

    for code, subject in subject_map.items():
        subject.done(clean_param=clean_param)

    return subject_map


def read_complete_dataset(filename="BBB_data_from_Rob.csv", clean_param=0):
    """Reads a data file and returns a list of Subjects.

    Data from personal correspondence with Rob Dunn, received 2-7-13.
    Converted from xlsx to csv.

    filename: string filename to read
    clean_param: parameter passed to Clean

    Returns: map from code to Subject
    """
    fp = open(filename)
    reader = csv.reader(fp)
    header = next(reader)
    logging.debug("%r", f"header={header}")
    header = next(reader)
    logging.debug("%r", f"header={header}")

    subject_codes = header[1:-1]
    subject_codes = ["B" + code for code in subject_codes]

    # create the subject map
    uber_subject = Subject("uber")
    subject_map = {}
    for code in subject_codes:
        subject_map[code] = Subject(code)

    # read lines
    i = 0
    for t in reader:
        otu_code = t[0]
        if otu_code == "":
            continue

        # pull out a species name and give it a number
        otu_names = t[-1]
        taxons = otu_names.split(";")
        species = taxons[-1]
        species = f"{species}-{i}"
        i += 1

        counts = [int(x) for x in t[1:-1]]

        # print otu_code, species

        for code, count in zip(subject_codes, counts):
            if count > 0:
                subject_map[code].add_species_count(species, count)
                uber_subject.add_species_count(species, count)

    uber_subject.done(clean_param=clean_param)
    for code, subject in subject_map.items():
        subject.done(clean_param=clean_param)

    return subject_map, uber_subject


def join_subjects():
    """Reads both datasets and computes their inner join.

    Finds all subjects that appear in both datasets.

    For subjects in the rarefacted dataset, looks up the total
    number of reads and stores it as total_reads.  num_reads
    is normally 400.
    
    Returns: map from code to Subject
    """

    # read the rarefacted dataset
    sampled_subjects = read_rarefacted_data()

    # read the complete dataset
    all_subjects, _ = read_complete_dataset()

    for code, subject in sampled_subjects.items():
        if code in all_subjects:
            match = all_subjects[code]
            subject.match(match)

    return sampled_subjects


def jitter_curve(curve, dx=0.2, dy=0.3):
    """Adds random noise to the pairs in a curve.

    dx and dy control the amplitude of the noise in each dimension.
    """
    curve = [
        (x + np.random.uniform(-dx, dx), y + np.random.uniform(-dy, dy)) for x, y in curve
    ]
    return curve


def offset_curve(curve, i, n, dx=0.3, dy=0.3):
    """Adds random noise to the pairs in a curve.

    i is the index of the curve
    n is the number of curves

    dx and dy control the amplitude of the noise in each dimension.
    """
    xoff = -dx + 2 * dx * i / (n - 1)
    yoff = -dy + 2 * dy * i / (n - 1)
    curve = [(x + xoff, y + yoff) for x, y in curve]
    return curve


def plot_curves(curves, root="species-rare"):
    """Plots a set of curves.

    curves is a list of curves; each curve is a list of (x, y) pairs.
    """
    thinkplot.clear_figure()
    color = "#225EA8"

    n = len(curves)
    for i, curve in enumerate(curves):
        curve = offset_curve(curve, i, n)
        xs, ys = zip(*curve)
        thinkplot.plot_line(xs, ys, color=color, alpha=0.3, linewidth=0.5)

    thinkplot.save_plot(
        root=root, xlabel="# samples", ylabel="# species", formats=FORMATS, legend=False
    )


def plot_conditionals(cdfs, root="species-cond"):
    """Plots cdfs of num_new conditioned on k.

    cdfs: list of Cdf
    root: string filename root
    """
    thinkplot.clear_figure()
    thinkplot.pre_plot(num=len(cdfs))

    thinkplot.plot_cdfs(cdfs)

    thinkplot.save_plot(root=root, xlabel="# new species", ylabel="Prob", formats=FORMATS)


def plot_frac_cdfs(cdfs, root="species-frac"):
    """Plots CDFs of the fraction of species seen.

    cdfs: map from k to CDF of fraction of species seen after k samples
    """
    thinkplot.clear_figure()
    color = "#225EA8"

    for k, cdf in cdfs.items():
        xs, ys = cdf.render()
        ys = [1 - y for y in ys]
        thinkplot.plot_line(xs, ys, color=color, linewidth=1)

        x = 0.9
        y = 1 - cdf.prob(x)
        pyplot.text(
            x,
            y,
            str(k),
            fontsize=9,
            color=color,
            horizontalalignment="center",
            verticalalignment="center",
            bbox=dict(facecolor="white", edgecolor="none"),
        )

    thinkplot.save_plot(
        root=root,
        xlabel="Fraction of species seen",
        ylabel="Probability",
        formats=FORMATS,
        legend=False,
    )


class Species(thinkbayes.Suite):
    """Represents hypotheses about the number of species."""

    def __init__(self, ns, conc=1, iters=1000):
        hypos = [thinkbayes.Dirichlet(n, conc) for n in ns]
        thinkbayes.Suite.__init__(self, hypos)
        self.iters = iters

    def update(self, data):
        """Updates the suite based on the data.

        data: list of observed frequencies
        """
        # call Update in the parent class, which calls Likelihood
        thinkbayes.Suite.update(self, data)

        # update the next level of the hierarchy
        for hypo in self.values():
            hypo.update(data)

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under this hypothesis.

        hypo: Dirichlet object
        data: list of observed frequencies
        """
        dirichlet = hypo

        # draw sample Likelihoods from the hypothetical Dirichlet dist
        # and add them up
        like = 0
        for _ in range(self.iters):
            like += dirichlet.likelihood(data)

        # correct for the number of ways the observed species
        # might have been chosen from all species
        m = len(data)
        like *= thinkbayes.binomial_coef(dirichlet.n, m)

        return like

    def dist_n(self):
        """Computes the distribution of n."""
        pmf = thinkbayes.Pmf()
        for hypo, prob in self.items():
            pmf.set(hypo.n, prob)
        return pmf


class Species2(object):
    """Represents hypotheses about the number of species.

    Combines two layers of the hierarchy into one object.

    ns and probs represent the distribution of N

    params represents the parameters of the Dirichlet distributions
    """

    def __init__(self, ns, conc=1.0, iters=1000):
        self.ns = ns
        self.conc = conc
        self.probs = np.ones(len(ns), dtype=np.float)
        self.params = np.ones(self.ns[-1], dtype=np.float) * conc
        self.iters = iters
        self.num_reads = 0
        self.m = 0

    def preload(self, data):
        """Change the initial parameters to fit the data better.

        Just an experiment.  Doesn't work.
        """
        m = len(data)
        singletons = data.count(1)
        num = m - singletons
        print(m, singletons, num)
        addend = np.ones(num, dtype=np.float) * 1
        print(len(addend))
        print(len(self.params[singletons:m]))
        self.params[singletons:m] += addend
        print("Preload", num)

    def update(self, data):
        """Updates the distribution based on data.

        data: numpy array of counts
        """
        self.num_reads += sum(data)

        like = np.zeros(len(self.ns), dtype=np.float)
        for _ in range(self.iters):
            like += self.sample_likelihood(data)

        self.probs *= like
        self.probs /= self.probs.sum()

        self.m = len(data)
        # self.params[:self.m] += data * self.conc
        self.params[: self.m] += data

    def sample_likelihood(self, data):
        """Computes the likelihood of the data for all values of n.

        Draws one sample from the distribution of prevalences.

        data: sequence of observed counts

        Returns: numpy array of m likelihoods
        """
        gammas = np.random.gamma(self.params)

        m = len(data)
        row = gammas[:m]
        col = np.cumsum(gammas)

        log_likes = []
        for n in self.ns:
            ps = row / col[n - 1]
            terms = np.log(ps) * data
            log_like = terms.sum()
            log_likes.append(log_like)

        log_likes -= np.max(log_likes)
        likes = np.exp(log_likes)

        coefs = [thinkbayes.binomial_coef(n, m) for n in self.ns]
        likes *= coefs

        return likes

    def dist_n(self):
        """Computes the distribution of n.

        Returns: new Pmf object
        """
        pmf = thinkbayes.Pmf(dict(zip(self.ns, self.probs)))
        return pmf

    def random_n(self):
        """Returns a random value of n."""
        return self.dist_n().random()

    def dist_q(self, iters=100):
        """Computes the distribution of q based on distribution of n.

        Returns: pmf of q
        """
        cdf_n = self.dist_n().make_cdf()
        sample_n = cdf_n.sample(iters)

        pmf = thinkbayes.Pmf()
        for n in sample_n:
            q = self.random_q(n)
            pmf.incr(q)

        pmf.normalize()
        return pmf

    def random_q(self, n):
        """Returns a random value of q.

        Based on n, self.num_reads and self.conc.

        n: number of species

        Returns: q
        """
        # generate random prevalences
        dirichlet = thinkbayes.Dirichlet(n, conc=self.conc)
        prevalences = dirichlet.random()

        # generate a simulated sample
        pmf = thinkbayes.Pmf(dict(enumerate(prevalences)))
        cdf = pmf.make_cdf()
        sample = cdf.sample(self.num_reads)
        seen = set(sample)

        # add up the prevalence of unseen species
        q = 0
        for species, prev in enumerate(prevalences):
            if species not in seen:
                q += prev

        return q

    def marginal_beta(self, n, index):
        """Computes the conditional distribution of the indicated species.
        
        n: conditional number of species
        index: which species

        Returns: Beta object representing a distribution of prevalence.
        """
        alpha0 = self.params[:n].sum()
        alpha = self.params[index]
        return thinkbayes.Beta(alpha, alpha0 - alpha)

    def dist_of_prevalence(self, index):
        """Computes the distribution of prevalence for the indicated species.

        index: which species

        Returns: (metapmf, mix) where metapmf is a MetaPmf and mix is a Pmf
        """
        metapmf = thinkbayes.Pmf()

        for n, prob in zip(self.ns, self.probs):
            beta = self.marginal_beta(n, index)
            pmf = beta.make_pmf()
            metapmf.set(pmf, prob)

        mix = thinkbayes.make_mixture(metapmf)
        return metapmf, mix

    def sample_posterior(self):
        """Draws random n and prevalences.

        Returns: (n, prevalences)
        """
        n = self.random_n()
        prevalences = self.sample_prevalences(n)

        # print 'Peeking at n_cheat'
        # n = n_cheat

        return n, prevalences

    def sample_prevalences(self, n):
        """Draws a sample of prevalences given n.

        n: the number of species assumed in the conditional

        Returns: numpy array of n prevalences
        """
        if n == 1:
            return [1.0]

        q_desired = self.random_q(n)
        q_desired = max(q_desired, 0.000001)

        params = self.unbias(n, self.m, q_desired)

        gammas = np.random.gamma(params)
        gammas /= gammas.sum()
        return gammas

    def unbias(self, n, m, q_desired):
        """Adjusts the parameters to achieve desired prev_unseen (q).

        n: number of species
        m: seen species
        q_desired: prevalence of unseen species
        """
        params = self.params[:n].copy()

        if n == m:
            return params

        x = sum(params[:m])
        y = sum(params[m:])
        a = x + y
        # print x, y, a, x/a, y/a

        g = q_desired * a / y
        f = (a - g * y) / x
        params[:m] *= f
        params[m:] *= g

        return params


class Species3(Species2):
    """Represents hypotheses about the number of species."""

    def update(self, data):
        """Updates the suite based on the data.

        data: list of observations
        """
        # sample the likelihoods and add them up
        like = np.zeros(len(self.ns), dtype=np.float)
        for _ in range(self.iters):
            like += self.sample_likelihood(data)

        self.probs *= like
        self.probs /= self.probs.sum()

        m = len(data)
        self.params[:m] += data

    def sample_likelihood(self, data):
        """Computes the likelihood of the data under all hypotheses.

        data: list of observations
        """
        # get a random sample
        gammas = np.random.gamma(self.params)

        # row is just the first m elements of gammas
        m = len(data)
        row = gammas[:m]

        # col is the cumulative sum of gammas
        col = np.cumsum(gammas)[self.ns[0] - 1:]

        # each row of the array is a set of ps, normalized
        # for each hypothetical value of n
        array = row / col[:, np.newaxis]

        # computing the multinomial PDF under a log transform
        # take the log of the ps and multiply by the data
        terms = np.log(array) * data

        # add up the rows
        log_likes = terms.sum(axis=1)

        # before exponentiating, scale into a reasonable range
        log_likes -= np.max(log_likes)
        likes = np.exp(log_likes)

        # correct for the number of ways we could see m species
        # out of a possible n
        coefs = [thinkbayes.binomial_coef(n, m) for n in self.ns]
        likes *= coefs

        return likes


class Species4(Species):
    """Represents hypotheses about the number of species."""

    def update(self, data):
        """Updates the suite based on the data.

        data: list of observed frequencies
        """
        m = len(data)

        # loop through the species and update one at a time
        for i in range(m):
            one = np.zeros(i + 1)
            one[i] = data[i]

            # call the parent class
            Species.update(self, one)

    def likelihood(self, data, hypo):
        """Computes the likelihood of the data under this hypothesis.

        Note: this only works correctly if we update one species at a time.

        hypo: Dirichlet object
        data: list of observed frequencies
        """
        dirichlet = hypo
        like = 0
        for _ in range(self.iters):
            like += dirichlet.likelihood(data)

        # correct for the number of unseen species the new one
        # could have been
        m = len(data)
        num_unseen = dirichlet.n - m + 1
        like *= num_unseen

        return like


class Species5(Species2):
    """Represents hypotheses about the number of species.

    Combines two laters of the hierarchy into one object.

    ns and probs represent the distribution of N

    params represents the parameters of the Dirichlet distributions
    """

    def update(self, data):
        """Updates the suite based on the data.

        data: list of observed frequencies in increasing order
        """
        # loop through the species and update one at a time
        m = len(data)
        for i in range(m):
            self.update_one(i + 1, data[i])
            self.params[i] += data[i]

    def update_one(self, i, count):
        """Updates the suite based on the data.

        Evaluates the likelihood for all values of n.

        i: which species was observed (1..n)
        count: how many were observed
        """
        # how many species have we seen so far
        self.m = i

        # how many reads have we seen
        self.num_reads += count

        if self.iters == 0:
            return

        # sample the likelihoods and add them up
        likes = np.zeros(len(self.ns), dtype=np.float)
        for _ in range(self.iters):
            likes += self.sample_likelihood(i, count)

        # correct for the number of unseen species the new one
        # could have been
        unseen_species = [n - i + 1 for n in self.ns]
        likes *= unseen_species

        # multiply the priors by the likelihoods and renormalize
        self.probs *= likes
        self.probs /= self.probs.sum()

    def sample_likelihood(self, *args):
        """Computes the likelihood of the data under all hypotheses.

        i: which species was observed
        count: how many were observed
        """
        i, count = args
        # get a random sample of p
        gammas = np.random.gamma(self.params)

        # sums is the cumulative sum of p, for each value of n
        sums = np.cumsum(gammas)[self.ns[0] - 1:]

        # get p for the mth species, for each value of n
        ps = gammas[i - 1] / sums
        log_likes = np.log(ps) * count

        # before exponentiating, scale into a reasonable range
        log_likes -= np.max(log_likes)
        likes = np.exp(log_likes)

        return likes


def make_posterior(constructor, data, ns, conc=1, iters=1000):
    """Makes a suite, updates it and returns the posterior suite.

    Prints the elapsed time.

    data: observed species and their counts
    ns: sequence of hypothetical ns
    conc: concentration parameter
    iters: how many samples to draw

    Returns: posterior suite of the given type
    """
    suite = constructor(ns, conc=conc, iters=iters)

    # print constructor.__name__
    start = time.time()
    suite.update(data)
    end = time.time()
    print("Processing time", end - start)

    return suite


def plot_all_versions():
    """Makes a graph of posterior distributions of N."""
    data = [1, 2, 3]
    m = len(data)
    n = 20
    ns = range(m, n)

    for constructor in [Species, Species2, Species3, Species4, Species5]:
        suite = make_posterior(constructor, data, ns)
        pmf = suite.dist_n()
        pmf.label = f"{constructor.__name__}"
        thinkplot.plot_pmf_line(pmf)

    thinkplot.save_plot(root="species3", xlabel=SPECIES_LABEL, ylabel="Prob")


def plot_medium():
    """Makes a graph of posterior distributions of N."""
    data = [1, 1, 1, 1, 2, 3, 5, 9]
    m = len(data)
    n = 20
    ns = range(m, n)

    for constructor in [Species, Species2, Species3, Species4, Species5]:
        suite = make_posterior(constructor, data, ns)
        pmf = suite.dist_n()
        pmf.label = f"{constructor.__name__}"
        thinkplot.plot_pmf_line(pmf)

    thinkplot.show_plot()


def simple_dirichlet_example():
    """Makes a plot showing posterior distributions for three species.

    This is the case where we know there are exactly three species.
    """
    thinkplot.clear_figure()
    thinkplot.pre_plot(3)

    names = ["lions", "tigers", "bears"]
    data = [3, 2, 1]

    dirichlet = thinkbayes.Dirichlet(3)
    for i in range(3):
        beta = dirichlet.marginal_beta(i)
        print("mean", names[i], beta.mean())

    dirichlet.update(data)
    for i in range(3):
        beta = dirichlet.marginal_beta(i)
        print("mean", names[i], beta.mean())

        pmf = beta.make_pmf(label=names[i])
        thinkplot.plot_pmf_line(pmf)

    thinkplot.save_plot(
        root="species1", xlabel="Prevalence", ylabel="Prob", formats=FORMATS,
    )


def hierarchical_example():
    """Shows the posterior distribution of n for lions, tigers and bears.
    """
    ns = range(3, 30)
    suite = Species(ns, iters=8000)

    data = [3, 2, 1]
    suite.update(data)

    thinkplot.clear_figure()
    thinkplot.pre_plot(num=1)

    pmf = suite.dist_n()
    thinkplot.plot_pdf_line(pmf)
    thinkplot.save_plot(
        root="species2", xlabel=SPECIES_LABEL, ylabel="Prob", formats=FORMATS,
    )


def compare_hierarchical_example():
    """Makes a graph of posterior distributions of N."""
    data = [3, 2, 1]
    m = len(data)
    n = 30
    ns = range(m, n)

    constructors = [Species, Species5]
    iters = [1000, 100]

    for constructor, iters in zip(constructors, iters):
        suite = make_posterior(constructor, data, ns, iters)
        pmf = suite.dist_n()
        pmf.label = f"{constructor.__name__}"
        thinkplot.plot_pmf_line(pmf)

    thinkplot.show_plot()


def process_subjects(codes):
    """Process subjects with the given codes and plot their posteriors.

    code: sequence of string codes
    """
    thinkplot.clear_figure()
    thinkplot.pre_plot(len(codes))

    subjects = read_rarefacted_data()
    pmfs = []
    for code in codes:
        subject = subjects[code]

        subject.process()
        pmf = subject.suite.dist_n()
        pmf.label = subject.code
        thinkplot.plot_pmf_line(pmf)

        pmfs.append(pmf)

    print("ProbGreater", thinkbayes.pmf_prob_greater(pmfs[0], pmfs[1]))
    print("ProbLess", thinkbayes.pmf_prob_less(pmfs[0], pmfs[1]))

    thinkplot.save_plot(
        root="species4", xlabel=SPECIES_LABEL, ylabel="Prob", formats=FORMATS,
    )


def run_subject(code, conc=1, high=500):
    """Run the analysis for the subject with the given code.

    code: string code
    """
    subjects = join_subjects()
    subject = subjects[code]

    subject.process(conc=conc, high=high, iters=300)
    subject.make_quick_prediction()

    print_summary(subject)
    actual_l = subject.total_species - subject.num_species
    cdf_l = subject.dist_l().make_cdf()
    print_prediction(cdf_l, actual_l)

    subject.make_figures()

    num_reads = 400
    curves = subject.run_simulations(100, num_reads)
    root = "species-rare-%s" % subject.code
    plot_curves(curves, root=root)

    num_reads = 800
    curves = subject.run_simulations(500, num_reads)
    ks = [100, 200, 400, 800]
    cdfs = make_conditionals(curves, ks)
    root = "species-cond-%s" % subject.code
    plot_conditionals(cdfs, root=root)

    num_reads = 1000
    curves = subject.run_simulations(500, num_reads, frac_flag=True)
    ks = [10, 100, 200, 400, 600, 800, 1000]
    cdfs = make_frac_cdfs(curves, ks)
    root = "species-frac-%s" % subject.code
    plot_frac_cdfs(cdfs, root=root)


def print_summary(subject):
    """Print a summary of a subject.

    subject: Subject
    """
    print(subject.code)
    print(f"found {subject.num_species} species in {subject.num_reads} reads")
    print(f"total {subject.total_species} species in {subject.total_reads} reads")

    cdf = subject.suite.dist_n().make_cdf()
    print("n")
    print_prediction(cdf, "unknown")


def print_prediction(cdf, actual):
    """Print a summary of a prediction.

    cdf: predictive distribution
    actual: actual value
    """
    median = cdf.percentile(50)
    low, high = cdf.credible_interval(75)

    print("predicted %0.2f (%0.2f %0.2f)" % (median, low, high))
    print("actual", actual)


def random_seed(x):
    """Initialize random.random and numpy.random.

    x: int seed
    """
    np.random.seed(x)


def generate_fake_sample(n, r, tr, conc=1.0):
    """Generates fake data with the given parameters.

    n: number of species
    r: number of reads in subsample
    tr: total number of reads
    conc: concentration parameter

    Returns: hist of all reads, hist of subsample, prev_unseen
    """
    # generate random prevalences
    dirichlet = thinkbayes.Dirichlet(n, conc=conc)
    prevalences = dirichlet.random()
    prevalences.sort()

    # generate a simulated sample
    pmf = thinkbayes.Pmf(dict(enumerate(prevalences)))
    cdf = pmf.make_cdf()
    sample = cdf.sample(tr)

    # collect the species counts
    hist = thinkbayes.Hist(sample)

    # extract a subset of the data
    if tr > r:
        np.random.shuffle(sample)
        subsample = sample[:r]
        subhist = thinkbayes.Hist(subsample)
    else:
        subhist = hist

    # add up the prevalence of unseen species
    prev_unseen = 0
    for species, prev in enumerate(prevalences):
        if species not in subhist:
            prev_unseen += prev

    return hist, subhist, prev_unseen


def plot_actual_prevalences():
    """Makes a plot comparing actual prevalences with a model.
    """
    # read data
    subject_map, _ = read_complete_dataset()

    # for subjects with more than 50 species,
    # PMF of max prevalence, and PMF of max prevalence
    # generated by a simulation
    pmf_actual = thinkbayes.Pmf()
    pmf_sim = thinkbayes.Pmf()

    # concentration parameter used in the simulation
    conc = 0.06

    for code, subject in subject_map.items():
        prevalences = subject.get_prevalences()
        m = len(prevalences)
        if m < 2:
            continue

        actual_max = max(prevalences)
        print(code, m, actual_max)

        # incr the PMFs
        if m > 50:
            pmf_actual.incr(actual_max)
            pmf_sim.incr(simulate_max_prev(m, conc))

    # plot CDFs for the actual and simulated max prevalence
    cdf_actual = pmf_actual.make_cdf(label="actual")
    cdf_sim = pmf_sim.make_cdf(label="sim")

    thinkplot.plot_cdfs([cdf_actual, cdf_sim])
    thinkplot.show_plot()


def scatter_prevalences(ms, actual):
    """Make a scatter plot of actual prevalences and expected values.

    ms: sorted sequence of in m (number of species)
    actual: sequence of actual max prevalence
    """
    for conc in [1, 0.5, 0.2, 0.1]:
        expected = [expected_max_prev(m, conc) for m in ms]
        thinkplot.plot_line(ms, expected)

    thinkplot.scatter_plot(ms, actual)
    thinkplot.show_plot(xscale="log")


def simulate_max_prev(m, conc=1.0):
    """Returns random max prevalence from a Dirichlet distribution.

    m: int number of species
    conc: concentration parameter of the Dirichlet distribution

    Returns: float max of m prevalences
    """
    dirichlet = thinkbayes.Dirichlet(m, conc)
    prevalences = dirichlet.random()
    return max(prevalences)


def expected_max_prev(m, conc=1, iters=100):
    """Estimate expected max prevalence.

    m: number of species
    conc: concentration parameter
    iters: how many iterations to run

    Returns: expected max prevalence
    """
    dirichlet = thinkbayes.Dirichlet(m, conc)

    t = []
    for _ in range(iters):
        prevalences = dirichlet.random()
        t.append(max(prevalences))

    return np.mean(t)


class Calibrator(object):
    """Encapsulates the calibration process."""

    def __init__(self, conc=0.1):
        """
        """
        self.conc = conc

        self.ps = range(10, 100, 10)
        self.total_n = np.zeros(len(self.ps))
        self.total_q = np.zeros(len(self.ps))
        self.total_l = np.zeros(len(self.ps))

        self.n_seq = []
        self.q_seq = []
        self.l_seq = []

    def calibrate(self, num_runs=100, n_low=30, n_high=400, r=400, tr=1200):
        """Runs calibrations.

        num_runs: how many runs
        """
        for seed in range(num_runs):
            self.run_calibration(seed, n_low, n_high, r, tr)

        self.total_n *= 100.0 / num_runs
        self.total_q *= 100.0 / num_runs
        self.total_l *= 100.0 / num_runs

    def validate(self, num_runs=100, clean_param=0):
        """Runs validations.

        num_runs: how many runs
        """
        subject_map, _ = read_complete_dataset(clean_param=clean_param)

        i = 0

        for match in subject_map.values():
            if match.num_reads < 400:
                continue
            num_reads = 100

            print("Validate", match.code)
            subject = match.resample(num_reads)
            subject.match(match)

            n_actual = None
            q_actual = subject.prev_unseen
            l_actual = subject.total_species - subject.num_species
            self.run_subject(subject, n_actual, q_actual, l_actual)

            i += 1
            if i == num_runs:
                break

        self.total_n *= 100.0 / num_runs
        self.total_q *= 100.0 / num_runs
        self.total_l *= 100.0 / num_runs

    def plot_n(self, root="species-n"):
        """Makes a scatter plot of simulated vs actual prev_unseen (q).
        """
        xs, ys = zip(*self.n_seq)
        if None in xs:
            return

        high = max(xs + ys)

        thinkplot.plot_line([0, high], [0, high], color="gray")
        thinkplot.scatter_plot(xs, ys)
        thinkplot.save_plot(root=root, xlabel="Actual n", ylabel="Predicted")

    def plot_q(self, root="species-q"):
        """Makes a scatter plot of simulated vs actual prev_unseen (q).
        """
        thinkplot.plot_line([0, 0.2], [0, 0.2], color="gray")
        xs, ys = zip(*self.q_seq)
        thinkplot.scatter_plot(xs, ys)
        thinkplot.save_plot(root=root, xlabel="Actual q", ylabel="Predicted")

    def plot_l(self, root="species-n"):
        """Makes a scatter plot of simulated vs actual l.
        """
        thinkplot.plot_line([0, 20], [0, 20], color="gray")
        xs, ys = zip(*self.l_seq)
        thinkplot.scatter_plot(xs, ys)
        thinkplot.save_plot(root=root, xlabel="Actual l", ylabel="Predicted")

    def plot_calibration_curves(self, root="species5"):
        """Plots calibration curves"""
        print(self.total_n)
        print(self.total_q)
        print(self.total_l)

        thinkplot.plot_line([0, 100], [0, 100], color="gray", alpha=0.2)

        if self.total_n[0] >= 0:
            thinkplot.plot_line(self.ps, self.total_n, label="n")

        thinkplot.plot_line(self.ps, self.total_q, label="q")
        thinkplot.plot_line(self.ps, self.total_l, label="l")

        thinkplot.save_plot(
            root=root,
            axis=[0, 100, 0, 100],
            xlabel="Ideal percentages",
            ylabel="Predictive distributions",
            formats=FORMATS,
        )

    def run_calibration(self, seed, n_low, n_high, r, tr):
        """Runs a single calibration run.

        Generates N and prevalences from a Dirichlet distribution,
        then generates simulated data.

        Runs analysis to get the posterior distributions.
        Generates calibration curves for each posterior distribution.

        seed: int random seed
        """
        # generate a random number of species and their prevalences
        # (from a Dirichlet distribution with alpha_i = conc for all i)
        random_seed(seed)
        n_actual = np.random.randint(n_low, n_high + 1)

        hist, subhist, q_actual = generate_fake_sample(n_actual, r, tr, self.conc)

        l_actual = len(hist) - len(subhist)
        print("Run low, high, conc", n_low, n_high, self.conc)
        print("Run r, tr", r, tr)
        print("Run n, q, l", n_actual, q_actual, l_actual)

        # extract the data
        data = [count for species, count in subhist.items()]
        data.sort()
        print("data", data)

        # make a Subject and process
        subject = Subject("simulated")
        subject.num_reads = r
        subject.total_reads = tr

        for species, count in subhist.items():
            subject.add_species_count(species, count)
        subject.done()

        self.run_subject(subject, n_actual, q_actual, l_actual)

    def run_subject(self, subject, n_actual, q_actual, l_actual):
        """Runs the analysis for a subject.

        subject: Subject
        n_actual: number of species
        q_actual: prevalence of unseen species
        l_actual: number of new species
        """
        # process and make prediction
        subject.process(conc=self.conc, iters=100)
        subject.make_quick_prediction()

        # extract the posterior suite
        suite = subject.suite

        # check the distribution of n
        pmf_n = suite.dist_n()
        print("n")
        self.total_n += self.check_distribution(pmf_n, n_actual, self.n_seq)

        # check the distribution of q
        pmf_q = suite.dist_q()
        print("q")
        self.total_q += self.check_distribution(pmf_q, q_actual, self.q_seq)

        # check the distribution of additional species
        pmf_l = subject.dist_l()
        print("l")
        self.total_l += self.check_distribution(pmf_l, l_actual, self.l_seq)

    def check_distribution(self, pmf, actual, seq):
        """Checks a predictive distribution and returns a score vector.

        pmf: predictive distribution
        actual: actual value
        seq: which sequence to append (actual, mean) onto
        """
        mean = pmf.mean()
        seq.append((actual, mean))

        cdf = pmf.make_cdf()
        print_prediction(cdf, actual)

        sv = score_vector(cdf, self.ps, actual)
        return sv


def score_vector(cdf, ps, actual):
    """Checks whether the actual value falls in each credible interval.
    
    cdf: predictive distribution
    ps: percentages to check (0-100)
    actual: actual value

    Returns: numpy array of 0, 0.5, or 1
    """
    scores = []
    for p in ps:
        low, high = cdf.credible_interval(p)
        _score = score(low, high, actual)
        scores.append(_score)

    return np.array(scores)


def score(low, high, n):
    """Score whether the actual value falls in the range.

    Hitting the posts counts as 0.5, -1 is invalid.

    low: low end of range
    high: high end of range
    n: actual value

    Returns: -1, 0, 0.5 or 1
    """
    if n is None:
        return -1
    if low < n < high:
        return 1
    if n == low or n == high:
        return 0.5
    else:
        return 0


def fake_subject(n=300, conc=0.1, num_reads=400, prevalences=None):
    """Makes a fake Subject.
    
    If prevalences is provided, n and conc are ignored.

    n: number of species
    conc: concentration parameter
    num_reads: number of reads
    prevalences: np array of prevalences (overrides n and conc)
    """
    # generate random prevalences
    if prevalences is None:
        dirichlet = thinkbayes.Dirichlet(n, conc=conc)
        prevalences = dirichlet.random()
        prevalences.sort()

    # generate a simulated sample
    pmf = thinkbayes.Pmf(dict(enumerate(prevalences)))
    cdf = pmf.make_cdf()
    sample = cdf.sample(num_reads)

    # collect the species counts
    hist = thinkbayes.Hist(sample)

    # extract the data
    data = [count for species, count in hist.items()]
    data.sort()

    # make a Subject and process
    subject = Subject("simulated")

    for species, count in hist.items():
        subject.add_species_count(species, count)
    subject.done()

    return subject


def plot_subject_cdf(code=None, clean_param=0):
    """Checks whether the Dirichlet model can replicate the data.
    """
    subject_map, uber_subject = read_complete_dataset(clean_param=clean_param)

    if code is None:
        subjects = subject_map.values()
        subject = np.random.choice(subjects)
        code = subject.code
    elif code == "uber":
        subject = uber_subject
    else:
        subject = subject_map[code]

    print(subject.code)

    m = subject.get_m()

    subject.process(high=m, conc=0.1, iters=0)
    print(subject.suite.params[:m])

    # plot the cdf
    options = dict(linewidth=3, color="blue", alpha=0.5)
    cdf = subject.make_cdf()
    thinkplot.plot_cdf_line(cdf, **options)

    options = dict(linewidth=1, color="green", alpha=0.5)

    # generate fake subjects and plot their CDFs
    for _ in range(10):
        prevalences = subject.suite.sample_prevalences(m)
        fake = fake_subject(prevalences=prevalences)
        cdf = fake.make_cdf()
        thinkplot.plot_cdf_line(cdf, **options)

    root = "species-cdf-%s" % code
    thinkplot.save_plot(
        root=root, xlabel="rank", ylabel="CDF", xscale="log", formats=FORMATS,
    )


def run_calibration(flag="cal", num_runs=100, clean_param=50):
    """Runs either the calibration or validation process.

    flag: string 'cal' or 'val'
    num_runs: how many runs
    clean_param: parameter used for data cleaning
    """
    cal = Calibrator(conc=0.1)

    if flag == "val":
        cal.validate(num_runs=num_runs, clean_param=clean_param)
    else:
        cal.calibrate(num_runs=num_runs)

    cal.plot_n(root="species-n-%s" % flag)
    cal.plot_q(root="species-q-%s" % flag)
    cal.plot_l(root="species-l-%s" % flag)
    cal.plot_calibration_curves(root="species5-%s" % flag)


def run_tests():
    """Runs calibration code and generates some figures."""
    run_calibration(flag="val")
    run_calibration(flag="cal")

    plot_subject_cdf("B1558.G", clean_param=50)
    plot_subject_cdf(None)


def main(script):
    logging.debug("%r", f"script={script}")
    random_seed(17)
    run_subject("B1242", conc=1, high=100)

    random_seed(17)
    simple_dirichlet_example()

    random_seed(17)
    hierarchical_example()


if __name__ == "__main__":
    main(*sys.argv)
