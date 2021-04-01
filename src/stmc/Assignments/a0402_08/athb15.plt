# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iaton_hb.eps"
set title "2d O3 model: 100*100 lattice at beta=1.5"
set xrange [0:2400]
set yrange [1:90]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 1800,37.4
plot "a03_2d100.d15" using 1:2 notitle with line 1,\
"a03_2d100.d15" using 1:2:3 title "L=100 1-hit Metropolis" with errorbars 1,\
"a03hb_2d100.d15" using 1:2 notitle with line 2,\
"a03hb_2d100.d15" using 1:2:3 title "L=100 heat bath" with errorbars 2
pause -1
