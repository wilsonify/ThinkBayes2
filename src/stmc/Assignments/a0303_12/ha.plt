#set term pos eps enhanced defaultplex "Helvetica" 26
#set output 'hist.eps'
set title "Put Figure Title Here"
set xlabel "act"
set ylabel "Histogram"
# set xrange [0.4:-2]
# set yrange [0:6000]
# set key -1.6,5600
plot 'h4d10q010.d' using 2:4 title "10**4" with line 3,\
'h4d10q010.d' using 2:4:5 notitle with errorbar 3
pause -1
