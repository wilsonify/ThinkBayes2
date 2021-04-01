#set term pos eps enhanced defaultplex "Helvetica" 26
#set output 'hist.eps'
set title "Put Figure Title Here"
set xlabel "<q=1>"
set ylabel "Histogram"
# set xrange [0.4:-2]
# set yrange [0:6000]
# set key -1.6,5600
plot 'm4d10q010_01.d' using 2:3 title "10**4" with line 3,\
'm4d10q010_01.d' using 2:3:4 notitle with errorbar 3
pause -1
