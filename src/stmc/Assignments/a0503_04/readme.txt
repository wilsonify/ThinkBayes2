
Cluster (and Metropolis) updating.
==================================

2d 10-state Potts, 20**2 lattice at beta=0.62. 

Metropolis and heat bath: See assignment a0303_08.

1. SW:  potts_histcl.f  nequi,nrpt,nmeas:   20000    64     10000

2. Wolff: potts_histw.f nequi,nrpt,nmeas: 2400000    64   1200000.

Results:

1. SW cluster:  
 actm =     0.321809352  +/-     0.000096850
 em   =    -0.887237406  +/-     0.000387399
                 
2. Wolff cluster:
 actm =     0.321763763  +/-     0.000066520
 em   =    -0.887055051  +/-     0.000266081

Gauss diff test:  Q=0.71 (actm)
================

Comparison of the error bars by means of the F-Test:
====================================================

64 data in each case, SW vs. Wolff: Q=0.033 (significant!).

Average cluster sizes:
======================

ana_clsw.f SW clusters:    clsizem  =  1.7769 +/- 0.0004
ana_clw.f Wolff clusters:  clwsizem =  3.6865 +/- 0.0017

Timing:
=======

Sun workstation hlrz21 (Oct 23 2001):

potts_histcl.f: 350.5s;  potts_histw.f: 332.7s. 

