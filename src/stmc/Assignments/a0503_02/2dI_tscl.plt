# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_tscl.eps'
# set title "2d Ising Model time series on an 80 x 80 lattice"
set xlabel "Steps"
set ylabel "e_s"
set xrange [0:40]
set yrange [-2:0]
set key 30,-0.15
plot\
'potts1_ts.d' using 1:2 title 'Metropolis Random  Start' with line 1,\
'potts1_ts.d' using 1:3 title 'Metropolis Ordered Start' with line 1,\
'potts_tscl.d' using 1:2 title 'SW Cluster Random  Start' with line 2,\
'potts_tscl.d' using 1:3 title 'SW Cluster Ordered Start' with line 2,\
'potts_tsw.d' using 1:2 title 'W Cluster Random  Start' with line 3,\
'potts_tsw.d' using 1:3 title 'W Cluster Ordered Start' with line 3,\
'2dI_ts_ferdi.d' using 1:2 notitle with line 1
pause -1
