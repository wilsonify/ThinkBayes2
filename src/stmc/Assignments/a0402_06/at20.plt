# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iathb.eps"
set xrange [0:265]
set yrange [1:20]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 220,17.4
plot "a10_2d020.d" using 1:2 notitle with line 1,\
"a10_2d020.d" using 1:2:3 title "L=40 1-hit Metropolis" with errorbars 1,\
"a10_2d020lm2.d" using 1:2 notitle with line 3,\
"a10_2d020lm2.d" using 1:2:3 title "L=40 2-hit Metropolis" with errorbars 3,\
"a10_2d020hb.d" using 1:2 notitle with line 5,\
"a10_2d020hb.d" using 1:2:3 title "L=40 heat bath" with errorbars 5
pause -1
