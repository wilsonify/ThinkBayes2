# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_muh.eps'
# set title "Put Figure Title Here"
set xlabel "e_s"
set ylabel "Histograms"
set xrange [0.4:-2.0]
set yrange [0:7500]
set key -1.05,5500
plot 'hmu2d02q020.d' using 1:2:3 title "multicanonical" with errorbar 1,\
 'hb2d02q020.d' using 1:2:3 title "beta=0.0" with errorbar 1,\
 'hb2d02q020.d' using 1:4:5 title "beta=0.2" with errorbar 2,\
 'hb2d02q020.d' using 1:6:7 title "beta=0.4" with errorbar 3
pause -1
