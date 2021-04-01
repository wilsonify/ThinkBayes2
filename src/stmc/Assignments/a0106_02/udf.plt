# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "udf.eps"
set key 0.45,0.92
set xlabel "x"
set ylabel "F and F_q"
plot "df01.d" using 1:2 title  "Empirical F and F_q" with line 1,\
     "df01.d" using 1:3 notitle with line 1,\
     "udf.d" using 1:2 title "Exact F and F_q"  with line 2,\
     "udf.d" using 1:3 notitle with line 2
pause -1
