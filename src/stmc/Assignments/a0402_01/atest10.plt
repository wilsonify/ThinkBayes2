# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat1.eps"
set xrange [0:265]
set yrange [0:37]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 224,15
plot "a02_2d010.d" using 1:2 notitle with line 5,\
"a02_2d010.d" using 1:2:3 title "L= 10" with errorbars 5,\
"a02_2d010m.d" using 1:2 notitle with line 4,\
"a02_2d010m.d" using 1:2:3 title "L= 10 m" with errorbars 4,\
"a02_2d010r.d" using 1:2 notitle with line 3,\
"a02_2d010r.d" using 1:2:3 title "L= 10 r" with errorbars 3
pause -1
