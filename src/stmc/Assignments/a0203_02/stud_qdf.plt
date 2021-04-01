# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "stud_qdf.eps"
set noyzeroaxis
set xlabel "t"
set ylabel "F_q_N(t)"
# set yrange [0:0.52]
# set key -2,0.9
plot 'fqdf1.d' using 1:2 notitle with line 1,\
     'fqdf1.d' using 1:3 notitle with line 1,\
     'fqdf1.d' using 1:4 notitle with line 1,\
     'fqdf1.d' using 1:5 notitle with line 1,\
     'fqdf2.d' using 1:2 notitle with line 1,\
     'fqdf2.d' using 1:3 notitle with line 1,\
     'fqdf2.d' using 1:4 notitle with line 1,\
     'fqdf2.d' using 1:5 notitle with line 1
pause -1
