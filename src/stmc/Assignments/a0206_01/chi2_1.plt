# Gnuplot Template.
# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'chi2_1.eps'
# set title "Put Figure Title Here"
# set xrange [0:2]
# set logscale y
set xlabel "Q"
set ylabel "F^p"
set key 0.15,0.46
plot "chi2_1.d" using 1:3 title "f=5" with line 1,\
     "chi2_1.d" using 2:3 title "f=6" with line 3
pause -1
