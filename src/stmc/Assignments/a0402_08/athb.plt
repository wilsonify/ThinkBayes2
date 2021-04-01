# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iaton_hb.eps"
set xrange [0:200]
set yrange [1:13]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 180,7.4
plot "a03_2d100.d" using 1:2 notitle with line 1,\
"a03_2d100.d" using 1:2:3 title "L=100 1-hit Metropolis" with errorbars 1,\
"a03hb_2d100.d" using 1:2 notitle with line 2,\
"a03hb_2d100.d" using 1:2:3 title "L=100 heat bath" with errorbars 2
pause -1
