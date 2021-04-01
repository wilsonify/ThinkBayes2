set term pos eps enhanced defaultplex "Helvetica" 26
set output 'tau_lin.eps'
reset
set xrange [-0.2:9.3]
set yrange [-5.5:0.3]
set label "a_1 = - 0.016 (08)" at 5.7,-0.8
set label "a_2 = - 0.534 (11)" at 5.7,-1.3
set xlabel "t"
set ylabel "ln [ C(t) ]"
set label "|" at -1.8,-2.55
plot 'lfit.d1' using 1:2:3 title "Data" with errorbar,\
     'lfit.d2' using 1:2 notitle with line 1,\
     'lfit.d2' using 1:3 title "Linear fit" with line 1,\
     'lfit.d2' using 1:4 notitle with line 1
# pause -1
