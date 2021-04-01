#set term pos eps enhanced defaultplex "Helvetica" 26
#set output '2dI_es.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "actm"
set xrange [0:1.0]
# set yrange [0.5:1.01]
set key 0.30,0.92
plot 'a2d10q020.d' using 1:3:4 title 'MUCA histograms' with errorbar 1,\
'a2d10q020lts.d' using 1:3:4 title 'MUCA time series' with errorbar 2
pause -1
