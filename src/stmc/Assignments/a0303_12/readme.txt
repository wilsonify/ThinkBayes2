
1. Compile and run Hvalue.f to get the H-value for mc.par.
   This program calculates also the exact actm value.

2. Compile and run potts_hihb.f.

3. ana_hist.f give actm=0.27771 (40) versus actm=0.27778 (exact).
   The Gaussian difference test gives Q=0.87.
   Plot with ha.plt.

4. ana_mag.f give the probability 0.49951 (71) for q_i=1.
   The Gaussian difference test versus 0.5 (exact) gives Q=0.49.
   Plot with hm1.plt and hm2.plt.

   Why no equilibrium sweeps: At beta=0 the heat bath algorithm 
   is for arbitrary H immediately in equilibrium, as the field 
   is external, and the exact distribution for a given external
   field is sampled by this algorithm.
