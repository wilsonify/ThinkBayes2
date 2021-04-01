# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "chi2_pd.eps"
set noyzeroaxis
set xlabel "{/Symbol c}^2"
set ylabel "f_N({/Symbol c}^2)"
set yrange [0:1.4]
# set key -2,0.9
plot 'fpd1.d' using 1:2 notitle with line 1,\
     'fpd1.d' using 1:3 notitle with line 1,\
     'fpd1.d' using 1:4 notitle with line 1,\
     'fpd1.d' using 1:5 notitle with line 1,\
     'fpd2.d' using 1:2 notitle with line 1,\
     'fpd2.d' using 1:3 notitle with line 1,\
     'fpd2.d' using 1:4 notitle with line 1,\
     'fpd2.d' using 1:5 notitle with line 1,\
     'fpd3.d' using 1:2 notitle with line 1,\
     'fpd3.d' using 1:3 notitle with line 1,\
     'fpd3.d' using 1:4 notitle with line 1,\
     'fpd3.d' using 1:5 notitle with line 1,\
     'fpd4.d' using 1:2 notitle with line 1,\
     'fpd4.d' using 1:3 notitle with line 1,\
     'fpd4.d' using 1:4 notitle with line 1,\
     'fpd4.d' using 1:5 notitle with line 1,\
     'fpd5.d' using 1:2 notitle with line 1,\
     'fpd5.d' using 1:3 notitle with line 1,\
     'fpd5.d' using 1:4 notitle with line 1,\
     'fpd5.d' using 1:5 notitle with line 1
pause -1
