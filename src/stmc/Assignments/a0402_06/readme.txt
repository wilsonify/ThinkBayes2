
1. Compile and run the production programs
    p_met_ts.f, p_metn_ts.f and p_hb_ts.f.

2. Compile and run the analysis prgrams.
   ana_ts_p2.f is the ana_ts_p.f version for 2-hit Metropolis.

3. Plot L=20 test lattice with at20.plt, otherwise with athb.plt.


Results:
========

                    L=40                     L=80

1-hit:        18.96  (13) at t=120,    18.649 (88) at t=136.
2-hit:         9.854 (48) at t=096,     9.837 (44) at t=080.
heat bath:     2.767 (18) at t=112,     2.781 (20) at t=144.

1-hit/hb:      6.852 (65)               6.705 (58)
1-hit/2-hit:   1.924 (17)               1.896 (13)
