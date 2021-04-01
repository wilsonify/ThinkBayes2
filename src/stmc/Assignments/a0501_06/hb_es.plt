# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2d10q_muhes.eps'
# set title "Put Figure Title Here"
set xlabel "e_s"
set ylabel "Histograms"
set xrange [-3.5:-1.0]
set yrange [40:1800]
set logscale y
set key -1.80,660
plot 'hmu2d10q020.d' using 2:3:4 title "Multicanonical" with errorbar 1,\
 'hb2d10q020.d' using 2:5:6 title "Canonical {/Symbol b}=0.71" with errorbar 3
pause -1
