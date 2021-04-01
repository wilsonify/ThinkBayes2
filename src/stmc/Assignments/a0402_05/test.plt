# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat_acpt.eps"
set xrange [0:420]
set yrange [1:16.5]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 300,6
plot "a02_2d020l2b2ts1.d" using 1:2 notitle with line 1,\
"a02_2d020l2b2ts1.d" using 1:2:3 title "O2 19\% L=20" with errorbars 1,\
"a02_2d020l5b2ts1.d" using 1:2 notitle with line 5,\
"a02_2d020l5b2ts1.d" using 1:2:3 title "O2 50\% L=20" with errorbars 5
pause -1
