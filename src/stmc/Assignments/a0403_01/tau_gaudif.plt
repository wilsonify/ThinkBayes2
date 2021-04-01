# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'tau_gaudif.eps'
reset
set xrange [-0.2:9.3]
set yrange[-0.03:1.03]
set xlabel "t"
set ylabel "Q(t)"
plot 'gaudif.d' using 1:5 notitle with line 1
# pause -1
