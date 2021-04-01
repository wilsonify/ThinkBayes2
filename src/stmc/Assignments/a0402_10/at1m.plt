# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2r.eps"
set xrange [0:17]
# set yrange [0:58]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 200,17
plot "a03_2d100.d" using 1:2 notitle with line 1,\
 "a03_2d100.d" using 1:2:3 title "L=14 average energies" with errorbars 1,\
 "a03_2d100m.d" using 1:2 notitle with line 2,\
 "a03_2d100m.d" using 1:2:3 title "L=14 average energies" with errorbars 2
pause -1
