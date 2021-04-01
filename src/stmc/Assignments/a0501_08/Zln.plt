#set term pos eps enhanced defaultplex "Helvetica" 26
#set output '2dI_es.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "log(Z)"
set xrange [0:1.03]
# set yrange [0.5:1]
set key 0.50,-0.22
plot 'ferdi.D' using 1:7 title 'ln Z' with line 1,\
'Z2d02q020.d' using 1:3:4 title 'MUCA histograms' with errorbar 1,\
'Z2d02q020lts.d' using 1:3:4 title 'MUCA time series' with errorbar 2
pause -1
