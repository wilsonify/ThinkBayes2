# Gnuplot Template.
# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'fig.eps'
# set title "Put Figure Title Here"
# set xrange [0:2]
# set logscale y
# set xlabel "x"
# set ylabel "y"
# set key 0.1,1.8
plot "chi2_2.d" using 1:2 notitle with line 1
pause -1
