# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'Potts_mag.eps'
# set title "Magnetization squared"
set xlabel "{/Symbol \142}"
set ylabel "M^2"
set xrange [0.0:1.00]
set yrange [0.5:1]
set key 0.2,0.95
plot 'M2d02q020lts.d' using 1:3 title "2d Ising" with line 1
pause -1
