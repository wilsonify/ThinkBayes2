# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "bin_his.eps"
set yrange [0:7.0]
set xrange [0.25:0.75]
set xlabel "x^r"
set ylabel "H"
plot "gaus.d" using 1:2 title "Normal distribution" with line 2,\
     "h01.d" using 1:2 title "Binned data" with line 1
pause -1
