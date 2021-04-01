# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iaton_hb.eps"
set title "2d O3 model: 100*100 lattice at beta=1.1"
set xrange [0:600]
set yrange [1:18]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 500,9.0
plot "a03hb_2d100m.d11" using 1:2 notitle with line 1,\
"a03hb_2d100m.d11" using 1:2:3 title "Averaged L=100" with errorbars 1,\
"a03hb_2d100.d11" using 1:2 notitle with line 2,\
"a03hb_2d100.d11" using 1:2:3 title "L=100" with errorbars 2
pause -1
