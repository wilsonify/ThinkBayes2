
This subdirectory contain programs and routines for

Potts Model Monte Carlo Simulations
===================================

potts_hist.f    Using potts_met.f Metropolis updating.
                nequi      euilibrium sweeps followed by
                nrpt*nmeas measurement of energy (action) histograms.
potts1_hist.f   As potts_hist.f, but using potts1_met.f with potts1.com.

potts_ts.f      Time series for the early sweeps, systematic updating.
potts_ts_r.f    The same, but with random updating.

EDS:
===

p_e_cases.f:   Counts EDS cases - cases.r.
p_e_hist.f:    Production program as above.
p_e_init.f:    EDS Initialization routines.
p_e_mc.f:      MC update routines.
p_eds.par:     EDS parameter file.

Cluster:
=======
