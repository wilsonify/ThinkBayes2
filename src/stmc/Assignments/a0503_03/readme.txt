
Cluster (and Metropolis) updating.
==================================

2d Ising, 20**2 lattice at beta=0.4. 

Metropolis and heat bath: See assignment a0303_06.

1. SW:    potts_histcl.f  nequi=5 000,   nrpt=64, nmeas=2 500

2. Wolff: potts_histw.f.  nequi=7*5 000, nrpt=64, nmeas=7*2 500

Results from ana_hist.f:
                                                          Gauss diff test

Exact:            actm =  0.779458
                  em   = -1.117834
1. SW cluster:    actm =  0.779380141 +/- 0.000160505        
                  em   = -1.117520562 +/- 0.000642021        Q=0.63
2. Wolff cluster: actm =  0.779549368 +/- 0.000115166
                  em   = -1.118197473 +/- 0.000460663        Q=0.44

Comparison of the error bars by means of the F-Test:
====================================================

64 data in each case, SW 0.00065 vs. Wolff 0.00047: Q=0.01 (significant!).

Average cluster sizes:
======================

ana_clsw.f SW clusters:    clsizem  =  4.8121 +/- 0.0039
ana_clw.f Wolff clusters:  clwsizem = 57.3415 +/- 0.1045

Timing:
=======

Sun workstation hlrz21 (Oct 18 2001):

potts_histcl.f 105.2s;  potts_histw.f 82.2s. 

