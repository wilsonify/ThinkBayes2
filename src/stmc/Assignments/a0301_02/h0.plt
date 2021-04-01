# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_H_E.eps'
# set title "Put Figure Title Here"
set xlabel "e_s"
set ylabel "H"
set xrange [0.4:-2]
set yrange [0:6000]
set key -1.6,5600
plot 'I2d_H0.d' using 2:5 title "Random Sampling" with line 1,\
'I2d_H0.d' using 2:5:6 notitle with errorbar 1
pause -1
