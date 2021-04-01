
Single sweep averages of the action variable
============================================

Estimate of the integrated autocorrelation time and
resulting error bars on a 100**2 lattice.

Runs with nrpt=20, nmeas=nequi=2**15 for o3_tshb.f.

     Produce o03*.d data files -> renamed as follows:

beta=1.1:   o03*.d11 data file kept.

beta=1.5:   o03*.d15 data file kept.

Gnuplot *.plt files.

Improvement due to one-sweep averages:

Improvement Ration = tau_int_s*sigma0_s / (tau_int_a*sigma0*a)

Here the subscript s denotes one-sweep single measurements and 
the subscript a one-sweep averaged measurements. Ratio 
calculation with ratio.f.

beta        1.1                          1.5

tau_int_s   2.968 (57)                   13.09 (44)
sigma0_s    0.23253E-04 (0.00064E-04)    0.18562E-04 (0.00052E-04)

tau_int_a   3.738 (69)                   15.95 (54)
sigma0_a    0.18433E-04 (0.00066E-04)    0.15226E-04 (0.00054E-04)

ratio       1.002                        1.001

-------------------------------------------------

Metropolis simulations at beta=0.01:
====================================

Run on_ts.f and on_tsm.f at beta=0.01.

Analyse the MC data with ana_ts_onm.f and plot with at1m.plt.

