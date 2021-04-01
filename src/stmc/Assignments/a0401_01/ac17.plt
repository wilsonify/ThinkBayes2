# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'acor.eps'
# set title "Put Figure Title Here"
set xrange [-1.0:33.0]
set yrange [-0.05:1.05]
set xlabel "t"
set ylabel "C(t)"
set label " 0.08" at 5.5,0.91
set label "{/Symbol -}" at 9.4,0.91
set label " 0.00" at 5.5,0.51
set label "{/Symbol -}" at 9.4,0.51
set label "{/Symbol -} 0.08" at 5.0,0.11
set label "|" at -7.8,0.467
# set logscale y
plot "acor.d" using 1:2 notitle with line 1,\
     "acor.d" using 1:2:4 notitle with errorbar 1,\
 "acor.small" using 1:2 notitle with line 2,\
 "acor.small" using 1:2:3 notitle with errorbar 2,\
 "yaxis.small" using 1:2 notitle with line 1,\
    "uacor.d" using 1:2 notitle with line 3,\
    "uacor.d" using 1:2:4 notitle with errorbar 3
pause -1
