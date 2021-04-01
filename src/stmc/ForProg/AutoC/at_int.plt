# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'at_int.eps'
# set title "Put Figure Title Here"
set xrange [-0.5:32.5]
set yrange [0.80:4.1]
set xlabel "t"
set ylabel "{/Symbol t}_{int}"
# set logscale y
plot "at_int.d" using 1:2 notitle with line 1,\
     "at_int.d" using 1:2:4 notitle with errorbar 1,\
     "b_aint.d" using 2:4 notitle with line 3,\
     "b_aint.d" using 2:7 notitle with line 3,\
     "b_aint.d" using 2:8 notitle with line 3
pause -1
