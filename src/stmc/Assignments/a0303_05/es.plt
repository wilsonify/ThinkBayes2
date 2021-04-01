set term pos eps enhanced defaultplex "Helvetica" 26
set output '2d10qes_ts.eps'
# set title "2d 10-state Potts Model time series on an 80 x 80 lattice"
set xlabel "Sweeps"
set ylabel "e_{s}"
set xrange [-1:201]
set yrange [-4.0:0.5]
set label "Disordered starts" at 50.0,0.0 left
set label "Ordered starts" at 50.0,-3.5 left
set key 160,-2.0
plot 'potts_ts.d' using 1:2 title 'Metropolis 1-hit' with line 1,\
     'potts_ts.d' using 1:3 notitle with line 1,\
 'potts02_ts.d' using 1:2 title 'Metropolis 2-hit' with line 2,\
 'potts02_ts.d' using 1:3 notitle with line 2,\
 'pottshb_ts.d' using 1:2 title 'Heat Bath' with line 3,\
 'pottshb_ts.d' using 1:3 notitle with line 3
pause -1
