# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'b_ts.eps'
set xrange [0:1921]
set yrange [-0.2:7.2]
set xlabel "sweeps/nmea1"
set ylabel "i of {/Symbol b}_i"
plot "b_ts.d" using 1:2 notitle with line 1
pause -1
