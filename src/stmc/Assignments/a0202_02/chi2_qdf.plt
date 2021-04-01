# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "chi2_qdf.eps"
set noyzeroaxis
set xlabel "{/Symbol c}^2"
set ylabel "F_q_N({/Symbol c}^2)"
set yrange [0:0.52]
# set key -2,0.9
plot 'fqdf1.d' using 1:2 notitle with line 1,\
     'fqdf1.d' using 1:3 notitle with line 1,\
     'fqdf1.d' using 1:4 notitle with line 1,\
     'fqdf1.d' using 1:5 notitle with line 1,\
     'fqdf2.d' using 1:2 notitle with line 1,\
     'fqdf2.d' using 1:3 notitle with line 1,\
     'fqdf2.d' using 1:4 notitle with line 1,\
     'fqdf2.d' using 1:5 notitle with line 1,\
     'fqdf3.d' using 1:2 notitle with line 1,\
     'fqdf3.d' using 1:3 notitle with line 1,\
     'fqdf3.d' using 1:4 notitle with line 1,\
     'fqdf3.d' using 1:5 notitle with line 1,\
     'fqdf4.d' using 1:2 notitle with line 1,\
     'fqdf4.d' using 1:3 notitle with line 1,\
     'fqdf4.d' using 1:4 notitle with line 1,\
     'fqdf4.d' using 1:5 notitle with line 1,\
     'fqdf5.d' using 1:2 notitle with line 1,\
     'fqdf5.d' using 1:3 notitle with line 1,\
     'fqdf5.d' using 1:4 notitle with line 1,\
     'fqdf5.d' using 1:5 notitle with line 1
pause -1
