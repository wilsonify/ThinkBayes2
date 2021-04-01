# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "stud_pd.eps"
set noyzeroaxis
set xlabel "t"
set ylabel "f(t)"
set xrange [-3:3]
set yrange [0:0.41]
# set key -2,0.9
plot 'fpd1.d' using 1:2 notitle with line 1,\
     'fpd1.d' using 1:3 notitle with line 1,\
     'fpd1.d' using 1:4 notitle with line 1,\
     'fpd1.d' using 1:5 notitle with line 1,\
     'fpd2.d' using 1:2 notitle with line 1,\
     'fpd2.d' using 1:3 notitle with line 1,\
     'fpd2.d' using 1:4 notitle with line 1,\
     'fpd2.d' using 1:5 notitle with line 1
pause -1
