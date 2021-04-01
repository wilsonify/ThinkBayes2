set term pos eps enhanced defaultplex "Helvetica" 26
set output "iat2m.eps"
set xrange [0:265]
# set yrange [0:58]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 100,165
plot "a02_2d040m.d" using 1:2 notitle with line 1,\
"a02_2d040m.d" using 1:2:3 title "L=40 averaged" with errorbars 1,\
 "a02_2d040.d" using 1:2 notitle with line 2,\
 "a02_2d040.d" using 1:2:3 title "L=40  single  " with errorbars 2,\
"a02_2d020m.d" using 1:2 notitle with line 3,\
"a02_2d020m.d" using 1:2:3 title "L=20 averaged" with errorbars 3,\
 "a02_2d020.d" using 1:2 notitle with line 4,\
 "a02_2d020.d" using 1:2:3 title "L=20  single  " with errorbars 4
# pause -1
