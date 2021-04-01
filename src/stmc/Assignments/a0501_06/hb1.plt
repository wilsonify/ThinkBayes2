# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2d10q_muh.eps'
# set title "Put Figure Title Here"
set xlabel "act"
set ylabel "Histograms"
set xrange [0.3:1.0]
set yrange [40:2000]
set logscale y
set key 0.71,730
plot 'hmu2d10q020.d' using 1:3:4 title "Multicanonical" with errorbar 1,\
'hb2d10q020.d' using 1:3:4 title "Canonical {/Symbol b}=0.7075" with errorbar 3
pause -1
