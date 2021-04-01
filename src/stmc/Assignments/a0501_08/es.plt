#set term pos eps enhanced defaultplex "Helvetica" 26
#set output '2dI_es.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "e_s"
set xrange [0:0.6]
set yrange [-2:0]
set key 0.50,-0.22
plot 'ferdi.D' using 1:3 title 'Energy per spin e_s' with line 1,\
'e2d02q020.d' using 1:3:4 title 'MUCA histograms' with errorbar 1,\
'e2d02q020lts.d' using 1:3:4 title 'MUCA time series' with errorbar 2
pause -1
