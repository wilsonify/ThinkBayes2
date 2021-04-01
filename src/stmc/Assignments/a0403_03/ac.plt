# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'ac.eps'
# set title "Put Figure Title Here"
set xrange [-1.0:11.0]
set yrange [-0.04:1.03]
set xlabel "t"
set ylabel "C(t)"
# set logscale y
plot "acor.d" using 1:2 notitle with line 1,\
     "acor.d" using 1:2:4 notitle with errorbar 1
pause -1
