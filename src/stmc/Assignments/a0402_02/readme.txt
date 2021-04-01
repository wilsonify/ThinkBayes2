
The critical beta_c is calculated by beta_c.f.

1. The production programs are p_met_ts.f, p_metr_ts.f and p_met_tsm.f.

2. The analysis programs are ana_ts_p.f, ana_ts_pr.f and ana_ts_pm.f.

3. Plot the results with at2.plt, at2r.plt and at2m.plt.

   For L=10 tests plot with atest10.plt.

4. To extend the analysis of ana_ts_p.f to much larger t values, so  
   that asymptotic tau_int estimates are obtained, its version
   ana_ts_plarge.f and the program ana_ts1_p.f are used with the
   following parameters: (large NT values take a lot of time)

   L=20 and  40:  NT=4096 and NSTEP=064.
   L=80 and 160:  NT=8192 and NSTEP=128.

   Output data are on the t*.d and t*.d2 files.

   Plot the 20 and 40 lattice with ta20_40.plt.

   Plot all thus obtained results with at2_l_all.plt, the ana_ts1_p.f 
   results alone, as used in the book, are plotted with at2_large.plt.

   Special run for L=160 with NT=32768 and NSTEP=128: 
   Results are on t*.d2long file. Plot with at2_160.plt.

The tau_int estimates at beta_c are collected in tau_int.D.
                                                 ===========
Fit of these data is the next assignment.
