# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "F_df.eps"
set noyzeroaxis
set xlabel "F"
set ylabel "F_{qdf}"
# set yrange [0:1.4]
# set key -2,0.9
plot 'F_df.d' using 1:2 notitle with line 1,\
     'F_df.d' using 1:3 notitle with line 2,\
     'F_df.d' using 1:4 notitle with line 3,\
     'F_df.d' using 1:5 notitle with line 1,\
     'F_df.d' using 1:6 notitle with line 2,\
     'F_df.d' using 1:7 notitle with line 3
pause -1
