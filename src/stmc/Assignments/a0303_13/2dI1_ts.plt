# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_ts.eps'
# set title "Put Figure Title Here"
set xlabel "Sweeps"
set ylabel "e_s"
set label "2d Ising Model time series on an 80 x 80 lattice" at 55,-0.70
set xrange [-1:201]
set yrange [-2:0]
set key 170,-0.15
plot 'potts_ts.d' using 1:2 title 'Random  Start' with line 1,\
     'potts_ts.d' using 1:3 title 'Ordered Start' with line 2,\
     'potts1_ts.d' using 1:2 title 'Random  Start' with line 3,\
     'potts1_ts.d' using 1:3 title 'Ordered Start' with line 4,\
     '2dI_ts_ferdi.d' using 1:2 title 'Exact' with line 3
pause -1
