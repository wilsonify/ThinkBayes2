reset
# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iathb.eps"
# set xrange [0:60]
# set yrange [1:4]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
# set key 220,5.0
set key left
plot "a02_3d014r.d" using 1:2 notitle with line 2,\
"a02_3d014r.d" using 1:2:3 title "L=14 random updating" with errorbars 2
pause -1
