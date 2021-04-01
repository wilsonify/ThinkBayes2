# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'Potts_M2.eps'
# set title "Magnetization squared"
set xlabel "{/Symbol \142}"
set ylabel "M^2_{q0}"
set xrange [0.0:1.00]
set yrange [0.0:1]
set key 0.35,0.35
plot '../a0501_08/M2d02q020lts.d' using 1:3 title "2d Ising" with line 1,\
     'M2d10q020lts.d' using 1:3 title "2d q=10 Potts" with line 3
pause -1
