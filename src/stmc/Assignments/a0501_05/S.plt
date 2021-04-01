# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_F.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "s"
set xrange [0.0:1.02]
# set yrange [-0.04:0.74]
set key 0.85,0.59
plot 'S2d10q020.d' using 1:3:4 notitle with errorbar 1,\
    'Sb2d10q020.d' using 1:3:4 notitle with errorbar 1
pause -1
