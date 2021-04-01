# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'entropy.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "s"
set xrange [0.0:1.00]
# set yrange [0:0.8]
set key 0.34,1.0
plot 'S2d10q020.d' using 1:3:4 title "MUCA histogram" with errorbar 1,\
'S2d10q020lts.d' using 1:3:4 title "MUCA time series" with errorbar 2
pause -1
