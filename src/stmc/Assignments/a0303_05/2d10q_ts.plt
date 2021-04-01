# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2d10q_ts.eps'
# set title "2d 10-state Potts Model time series on an 80 x 80 lattice"
set xlabel "Sweeps"
set ylabel "act"
set xrange [-1:201]
set yrange [0.1:1.0]
set label "Ordered starts" at 30.0,0.9 left
set label "Disordered starts" at 30.0,0.2 left
set key 170,0.70
plot 'potts_ts.d' using 1:4 title 'Metropolis 1-hit' with line 1,\
     'potts_ts.d' using 1:5 notitle with line 1,\
 'potts02_ts.d' using 1:4 title 'Metropolis 2-hit' with line 2,\
 'potts02_ts.d' using 1:5 notitle with line 2,\
 'pottshb_ts.d' using 1:4 title 'Heat Bath' with line 3,\
 'pottshb_ts.d' using 1:5 notitle with line 3
pause -1
