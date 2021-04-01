# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_C.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "C"
set xrange [0.0:1.00]
# set yrange [0:1.8]
set key 0.32,4.59
plot 'C2d10q020.d' using 1:3:4 title "Specific heat" with errorbar 1
pause -1
