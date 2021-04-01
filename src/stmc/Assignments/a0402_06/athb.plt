# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iathb.eps"
set xrange [0:265]
set yrange [1:20]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 220,17.4
plot "a10_2d040.d" using 1:2 notitle with line 1,\
"a10_2d040.d" using 1:2:3 title "L=40 1-hit Metropolis" with errorbars 1,\
"a10_2d080.d" using 1:2 notitle with line 2,\
"a10_2d080.d" using 1:2:3 title "L=80 1-hit Metropolis" with errorbars 2,\
"a10_2d040lm2.d" using 1:2 notitle with line 3,\
"a10_2d040lm2.d" using 1:2:3 title "L=40 2-hit Metropolis" with errorbars 3,\
"a10_2d080lm2.d" using 1:2 notitle with line 4,\
"a10_2d080lm2.d" using 1:2:3 title "L=80 2-hit Metropolis" with errorbars 4,\
"a10_2d040hb.d" using 1:2 notitle with line 5,\
"a10_2d040hb.d" using 1:2:3 title "L=40 heat bath" with errorbars 5,\
"a10_2d080hb.d" using 1:2 notitle with line 6,\
"a10_2d080hb.d" using 1:2:3 title "L=80 heat bath" with errorbars 6
pause -1
