# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "F_pd.eps"
set noyzeroaxis
set xlabel "F"
set ylabel "F_{pd}"
set yrange [0:1.3]
# set key -2,0.9
plot 'F_pd.d' using 1:2 notitle with line 1,\
     'F_pd.d' using 1:3 notitle with line 2,\
     'F_pd.d' using 1:4 notitle with line 3
pause -1
