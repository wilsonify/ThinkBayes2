# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2.eps"
set xrange [0:3080]
set yrange [0:300]
set key 2700,240
plot "ta02_2d040.d" using 1:2 notitle with line 3,\
"ta02_2d040.d" using 1:2:3 title "L= 40" with errorbars 3,\
"ta02_2d020.d" using 1:2 notitle with line 4,\
"ta02_2d020.d" using 1:2:3 title "L= 20" with errorbars 4
pause -1
