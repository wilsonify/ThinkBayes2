# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2r.eps"
set xrange [0:265]
# set yrange [0:58]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 200,17
plot "a02_2d020m.d" using 1:2 notitle with line 1,\
"a02_2d020m.d" using 1:2:3 title "L=20 average energies" with errorbars 1,\
 "a02_2d020.d" using 1:2 notitle with line 2,\
 "a02_2d020.d" using 1:2:3 title "L=20 single energies " with errorbars 2,\
"a02_2d040m.d" using 1:2 notitle with line 3,\
"a02_2d040m.d" using 1:2:3 title "L=40 average energies" with errorbars 3,\
 "a02_2d040.d" using 1:2 notitle with line 4,\
 "a02_2d040.d" using 1:2:3 title "L=40 single energies " with errorbars 4
# "a02_2d010m.d" using 1:2 notitle with line 5,\
# "a02_2d010m.d" using 1:2:3 title "L=10 average energies" with errorbars 5,\
# "a02_2d010.d" using 1:2 notitle with line 6,\
# "a02_2d010.d" using 1:2:3 title "L=10 single energies " with errorbars 6
pause -1
