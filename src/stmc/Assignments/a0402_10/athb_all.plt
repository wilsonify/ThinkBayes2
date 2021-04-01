# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "o3hb_av.eps"
# set title "2d O3 model: 100*100 lattice "
set xrange [0:600]
set yrange [1:18]
set xlabel "t"
set ylabel "{/Symbol t}_{int}
set key 500,9.5
plot "a03hb_2d100m.d15" using 1:2 notitle with line 1,\
"a03hb_2d100m.d15" using 1:2:3 title "Averaged L=100, {/Symbol b}=1.5" with errorbars 1,\
"a03hb_2d100.d15" using 1:2 notitle with line 2,\
"a03hb_2d100.d15" using 1:2:3 title "L=100, {/Symbol b}=1.5" with errorbars 2,\
"a03hb_2d100m.d11" using 1:2 notitle with line 3,\
"a03hb_2d100m.d11" using 1:2:3 title "Averaged L=100, {/Symbol b}=1.1" with errorbars 3,\
"a03hb_2d100.d11" using 1:2 notitle with line 4,\
"a03hb_2d100.d11" using 1:2:3 title "L=100, {/Symbol b}=1.1" with errorbars 4
pause -1
