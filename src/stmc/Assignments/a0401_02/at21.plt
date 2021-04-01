# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'at_int.eps'
# set title "Put Figure Title Here"
set xrange [-1.0:128.0]
set yrange [0.80:4.1]
set xlabel "t"
set ylabel "{/Symbol t}_{int}"
# set logscale y
plot "at_int21.d" using 1:2 notitle with line 1,\
     "at_int21.d" using 1:2:4 notitle with errorbar 1,\
     "uat_int21.d" using 1:2 notitle with line 2,\
     "uat_int21.d" using 1:2:4 notitle with errorbar 2,\
     "b_aint21.d" using 2:4 notitle with line 3,\
     "b_aint21.d" using 2:5 notitle with line 3,\
     "b_aint21.d" using 2:6 notitle with line 3
pause -1
