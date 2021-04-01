# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_muh.eps'
# set title "Put Figure Title Here"
set xlabel "act"
set ylabel "Histogram"
set xrange [0.3:1.0]
# set yrange [0:7500]
set key -1.05,5500
plot 'hmu2d10q020.d' using 1:3:4 title "multicanonical" with errorbar 1
pause -1
