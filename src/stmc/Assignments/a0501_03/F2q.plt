# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'free_energy.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "f"
set xrange [0.05:1.0]
set yrange [-10.0:-1.5]
set key 0.85,-8.50
plot 'ferdi.d' using 1:2 notitle with line 1,\
'F2d02q020.d' using 1:3:4 title 'Ising' with errorbar 1
pause -1
