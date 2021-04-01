# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'free_energy.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "f"
set xrange [0.05:1.0]
set yrange [-10.0:-1.5]
set key 0.85,-8.50
plot 'F2d10q020.d' using 1:3:4 title '10-state Potts' with errorbar 1
pause -1
