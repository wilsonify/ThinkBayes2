# Gnuplot Template.
# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'K2CGU.eps'
# set title "Put Figure Title Here"
set xrange [0.1:1]
set yrange [0:1]
# set logscale y
set xlabel "{/Symbol D}"
set ylabel "Q"
set key 0.87,0.58
plot "K2C_12_16.d" using 2:3 title "N_1=12,N_2=16" with line 1,\
     "K2G_12_16.d" using 2:3 title "N_1=12,N_2=16" with line 2,\
     "K2U_12_16.d" using 2:3 title "N_1=12,N_2=16" with line 3
pause -1
