# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iathb.eps"
set xrange [0:265]
set yrange [1:105]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 220,17.4
plot "a02_3d014.dbc" using 1:2 notitle with line 1,\
"a02_3d014.dbc" using 1:2:3 title "L=40 1-hit Metropolis" with errorbars 1,\
"a02_3d014hb.dbc" using 1:2 notitle with line 5,\
"a02_3d014hb.dbc" using 1:2:3 title "L=40 heat bath" with errorbars 5
pause -1
