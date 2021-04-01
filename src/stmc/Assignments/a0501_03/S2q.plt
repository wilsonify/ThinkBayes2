# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'entropy.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "s"
set xrange [0.0:1.00]
set yrange [0:0.8]
set key 0.34,0.12
plot 'ferdi.d' using 1:5 notitle with line 1,\
 'S2d02q020.d' using 1:3:4 title "Ising" with errorbar 1
pause -1
