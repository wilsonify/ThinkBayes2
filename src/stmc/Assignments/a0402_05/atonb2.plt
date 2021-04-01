# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat_acpt.eps"
set xrange [0:420]
set yrange [1:16.5]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 300,6
plot "a02_2d020l0b2ts1.d" using 1:2 notitle with line 1,\
"a02_2d020l0b2ts1.d" using 1:2:3 title "O2 19\% L=20" with errorbars 1,\
"a02_2d020l3b2ts1.d" using 1:2 notitle with line 2,\
"a02_2d020l3b2ts1.d" using 1:2:3 title "O2 24\% L=20" with errorbars 2,\
"a02_2d020l1b2ts1.d" using 1:2 notitle with line 3,\
"a02_2d020l1b2ts1.d" using 1:2:3 title "O2 30\% L=20" with errorbars 3,\
"a02_2d020l4b2ts1.d" using 1:2 notitle with line 5,\
"a02_2d020l4b2ts1.d" using 1:2:3 title "O2 40\% L=20" with errorbars 5,\
"a02_2d020lb2ts1.d" using 1:2 notitle with line 4,\
"a02_2d020lb2ts1.d" using 1:2:3 title "O2 50% L=20" with errorbars 4
pause -1
