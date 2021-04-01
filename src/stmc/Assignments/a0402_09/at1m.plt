# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2r.eps"
set xrange [0:17]
# set yrange [0:58]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 200,17
plot "a02_3d014m.d" using 1:2 notitle with line 5,\
 "a02_3d014m.d" using 1:2:3 title "L=14 average energies" with errorbars 5,\
 "a02_3d014.d" using 1:2 notitle with line 6,\
 "a02_3d014.d" using 1:2:3 title "L=14 single energies " with errorbars 6
pause -1
