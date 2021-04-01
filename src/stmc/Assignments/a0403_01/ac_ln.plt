# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'ac.eps'
# set title "Put Figure Title Here"
reset
# set xrange [-1.0:11.0]
# set yrange [0.003:1.05]
set xlabel "t"
set ylabel "ln[C(t)]"
plot "acln.d" using 1:2 notitle with line 1,\
     "acln.d" using 1:2:4 notitle with errorbar 1
pause -1
