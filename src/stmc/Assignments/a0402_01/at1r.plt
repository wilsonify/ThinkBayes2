# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat1r.eps"
set xrange [0:265]
set yrange [0:58]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 220,17
plot "a02_2d020r.d" using 1:2 notitle with line 1,\
"a02_2d020r.d" using 1:2:3 title "L=20 random updating    " with errorbars 1,\
"a02_2d040r.d" using 1:2 notitle with line 2,\
"a02_2d040r.d" using 1:2:3 title "L=40 random updating    " with errorbars 2,\
 "a02_2d020.d" using 1:2 notitle with line 3,\
 "a02_2d020.d" using 1:2:3 title "L=20 systematic updating" with errorbars 3,\
 "a02_2d040.d" using 1:2 notitle with line 4,\
 "a02_2d040.d" using 1:2:3 title "L=40 systematic updating" with errorbars 4
pause -1
