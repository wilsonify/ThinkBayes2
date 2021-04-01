# Gnuplot Template.
# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'kolm2.eps'
# set title "Put Figure Title Here"
set xrange [0.1:1]
set yrange [0:1]
# set logscale y
set xlabel "{/Symbol D}"
set ylabel "Q"
set key 0.87,0.58
plot "K2_12_16.d" using 2:3 title "N_1=12,N_2=16" with line 1,\
     "K2_06_08.d" using 2:3 title "N_1=6,N_2=8" with line 3,\
     "K2_03_05.d" using 2:3 title "N_1=3,N_2=5" with line 4,\
     "K2_01_02.d" using 2:3 title "N_1=1,N_2=2" with line 2
pause -1
