# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "at_cl.eps"
set xrange [0:65]
set yrange [1:14]
set xlabel "t"
set ylabel "{/Symbol t}_{int}"
set key 55,7.6
plot "a02_2d020cl.d" using 1:2 notitle with line 4,\
"a02_2d020cl.d" using 1:2:3 title "SW L= 20" with errorbars 4,\
"a02_2d020clw.d" using 1:2 notitle with line 8,\
"a02_2d020clw.d" using 1:2:3 title "W L= 20" with errorbars 8
pause -1
