
For 2dI_ts.plt:
===============

1. The value of the energy for an 80x80 Ising lattice at
   beta=0.4 is obtained with ferdi_2dI_ts.f on 2dI_ts_ferdi.d.

2. Run potts1_ts.f    -> potts1_ts.d   (Metropolis updating).

3. Run potts_ts_cl.f  -> potts_tscl.d  (SW cluster updating).

4. Run potts_ts_w.f   -> potts_tsw.d   (Woff cluster updating).

