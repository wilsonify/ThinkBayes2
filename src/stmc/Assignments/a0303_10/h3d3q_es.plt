set term pos eps enhanced defaultplex "Helvetica" 26
set output '3d3qes_h.eps'
# set title "Put Figure Title Here"
set xlabel "e_{0s}"
set ylabel "Histogram"
set xrange [-1.9:-1.0]
set yrange [0:65000]
set key -1.7,58000
plot "h3d03q024.d" using 3:4:5 title "h(e_{0s})" with errorbar
# pause -1
