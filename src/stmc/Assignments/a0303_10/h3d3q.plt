# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '3d3q_h.eps'
# set title "Put Figure Title Here"
set xlabel "actm"
set ylabel "Histogram"
set xrange [0.495:0.67]
set yrange [0:65000]
set key 0.63,58000
plot "h3d03q024.d" using 2:4:5 title "h(actm)" with errorbar
pause -1
