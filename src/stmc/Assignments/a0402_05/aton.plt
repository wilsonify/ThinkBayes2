# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iathb.eps"
# set xrange [0:265]
# set yrange [1:20]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 7000,200
plot "a02_2d060lts1.d" using 1:2 notitle with line 1,\
"a02_2d060lts1.d" using 1:2:3 title "O2 50% L=60" with errorbars 1,\
"a02_2d060l0ts1.d" using 1:2 notitle with line 2,\
"a02_2d060l0ts1.d" using 1:2:3 title "O2 32\% L=60" with errorbars 2,\
"a02_2d020lts1.d" using 1:2 notitle with line 3,\
"a02_2d020lts1.d" using 1:2:3 title "O2 50% L=20" with errorbars 3,\
"a02_2d020l0ts1.d" using 1:2 notitle with line 4,\
"a02_2d020l0ts1.d" using 1:2:3 title "O2 32\% L=20" with errorbars 4
pause -1
