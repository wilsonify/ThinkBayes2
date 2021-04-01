# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_C.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "C/N"
set xrange [0.0:1.00]
set yrange [0:1.9]
# set key 0.82,1.59
plot 'C2d02q020.d' using 1:3:4 title "Specific heat per spin" with errorbar 1,\
  'ferdi.d' using 1:6 notitle with line 1
pause -1
