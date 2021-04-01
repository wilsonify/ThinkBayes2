set term pos eps enhanced defaultplex "Helvetica" 26
set output 'tau_exp.eps'
reset
set xrange [-0.2:9.3]
set yrange [-0.03:1.03]
set label "a_1 = 0.984 (08)" at 6.0,0.8
set label "a_2 = 0.534 (11)" at 6.0,0.7
set xlabel "t"
set ylabel "C (t)"
set label "|" at -2.1,0.46
plot 'data.d' using 1:2:3 title "Data" with errorbar,\
     'gfit.d' using 1:2 title "Fit" with line 1
# pause -1
