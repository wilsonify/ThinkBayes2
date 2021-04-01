set term pos eps enhanced defaultplex "Helvetica" 26
set output "at_cl.eps"
set xrange [0:65]
set yrange [1:14]
set xlabel "t"
set ylabel "{/Symbol t}_{int}"
set key 55,7.6
plot "a02_2d160cl.d" using 1:2 notitle with line 1,\
"a02_2d160cl.d" using 1:2:3 title "SW L=160" with errorbars 1,\
"a02_2d080cl.d" using 1:2 notitle with line 2,\
"a02_2d080cl.d" using 1:2:3 title "SW L= 80" with errorbars 2,\
"a02_2d040cl.d" using 1:2 notitle with line 3,\
"a02_2d040cl.d" using 1:2:3 title "SW L= 40" with errorbars 3,\
"a02_2d020cl.d" using 1:2 notitle with line 4,\
"a02_2d020cl.d" using 1:2:3 title "SW L= 20" with errorbars 4,\
"a02_2d160clw.d" using 1:2 notitle with line 5,\
"a02_2d160clw.d" using 1:2:3 title "W L=160" with errorbars 5,\
"a02_2d080clw.d" using 1:2 notitle with line 6,\
"a02_2d080clw.d" using 1:2:3 title "W L= 80" with errorbars 6,\
"a02_2d040clw.d" using 1:2 notitle with line 7,\
"a02_2d040clw.d" using 1:2:3 title "W L= 40" with errorbars 7,\
"a02_2d020clw.d" using 1:2 notitle with line 8,\
"a02_2d020clw.d" using 1:2:3 title "W L= 20" with errorbars 8
# pause -1
