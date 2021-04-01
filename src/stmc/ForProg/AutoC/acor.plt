# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'acor.eps'
# set title "Put Figure Title Here"
set xrange [-1.0:33.0]
set yrange [-0.05:1.05]
set xlabel "t"
set ylabel "C(t)"
# set logscale y
plot "acor.d" using 1:2 notitle with line 1,\
     "acor.d" using 1:2:4 notitle with errorbar 1,\
    "uacor.d" using 1:2 notitle with line 3,\
    "uacor.d" using 1:2:4 notitle with errorbar 3
pause -1
