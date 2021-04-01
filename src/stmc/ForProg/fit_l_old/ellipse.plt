# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'ellipse.eps'
reset
set xlabel 'a_1'
set ylabel 'a_2'
# set xrange [0.65:1.60]
# set yrange [0.70:1.35]
plot "ellipse.d1" using 1:2 title 'Confidence Ellipse' with line 1,\
     "ellipse.d2" using 1:2 notitle with line 2
pause -1
