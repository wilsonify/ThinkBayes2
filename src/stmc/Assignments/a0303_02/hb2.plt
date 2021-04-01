#set term pos eps enhanced defaultplex "Helvetica" 26
#set output '2di_h_es.eps'
set title "Put Figure Title Here"
set xlabel "e_s"
set ylabel "Histograms"
set xrange [0.4:-2]
set yrange [0:6000]
# set key -1.6,5600
plot 'h2d02q2.d' using 3:4 title "beta=0.2" with line 3,\
'h2d02q2.d' using 3:4:5 notitle with errorbar 3
pause -1
