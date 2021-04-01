# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'bhanot.eps'
# set term pos color
# set output 'bhanot.ps'
set xrange [3.5:10.5]
set yrange [-0.00013:0.00012]
set xzeroaxis
set key 8.0,0.00010
plot "data2.d" using 1:5:3 title "Two parameter fit" with errorbar 2,\
   "data.d" using 1:5:3 title "Four parameter fit" with errorbar 1
pause -1
