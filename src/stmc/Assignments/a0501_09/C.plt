# set term pos eps enhanced defaultplex "Helvetica" 26
# set output '2dI_C.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "C"
set xrange [0.0:1.00]
# set yrange [0:10]
# set key 0.2,9.0
set key 0.2,100.0
plot 'C2d10q020.d' using 1:3:4 title "MUCA histograms" with errorbar 1,\
'C2d10q020lts.d' using 1:3:4 title "MUCA time series" with errorbar 2
pause -1
