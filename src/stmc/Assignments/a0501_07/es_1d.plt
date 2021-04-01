# set term pos eps enhanced defaultplex "Helvetica" 16
# set output '1dI_es.eps'
# set title "Energy per Spin vs Beta values"
set xlabel "{/Symbol \142}"
set ylabel "e_s"
set xrange [0:2]
set yrange [-1:0]
# set key 0.50,-0.22
plot 'E1d.d' using 1:2 title\
'Energy per spin e_s with open boundary conditions' with line 3,\
'E1d.d' using 1:3 title\
'Energy per spin e_s with periodic boundary conditions' with line 1,\
'e1d02q020.d' using 1:3:4 title 'Monte Carlo data' with errorbar 2
pause -1
