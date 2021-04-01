# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "qdf_o3.eps"
set xrange [0.35:0.51]
set yrange [0:0.520]
set xlabel "actm"
set ylabel "F_q"
plot "qdf_020.d" using 1:3 title "L=20" with line 1,\
     "qdf_034.d" using 1:3 title "L=34" with line 2,\
     "qdf_060.d" using 1:3 title "L=60" with line 3,\
     "qdf_100.d" using 1:3 title "L=100" with line 4
pause -1
