
Time Cluster (and Metropolis) updating routines.
================================================

2d Ising, 20**2 lattice at beta=0.4: nequi=50000, nrpt=0.

Compile and run potts_histcl.f, potts1_hist.f and potts_timew.f.


Sun workstation hlrz21 (Juelich Oct 19 2001):

1. SW clusters with potts_histcl.f:     32.5 s.
2. Metropolis with potts1_hist.f:       16.3 s.
3. Wolff clusters with potts_timew.f:   23.9 s. 
   Ration ncall/nequi in this part: 6.995.

Linux PC 600MHz lnx001.hep.fsu.edu (Oct 24 2001):

1. SW clusters with potts_histcl.f:     14.0 s.
2. Metropolis with potts1_hist.f:       07.9 s.
3. Wolff clusters with potts_timew.f:   09.8 s. 
   Ration ncall/nequi in this part: 6.995.
