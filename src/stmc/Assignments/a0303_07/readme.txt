
q=4 Potts,  120**2 lattice at beta=1.06722/2

nequi=400 000, nrpt=64, nmeas=200 000

1. Do assignment a0303_04 first, or:
   Time test run with nequi=400 and nmeas=200. Use optimization! 
   The real thing will take about 1000 times longer.

1. Compile and run potts_hist.f (in the background).

2. Compile and run ana_hist.f, the output is 
   actm =     0.646057365  +/-     0.000023375 .

   Caselle, et al. NPB 562 (99) 549-566: 0.64604 (6)

   Gauss difference test: Q=0.76.
