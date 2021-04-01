# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2.eps"
set xrange [0:34000]
set yrange [0:4000]
set key 25000,2000
plot "ta02_2d160.d2long" using 1:2 notitle with line 1,\
"ta02_2d160.d2long" using 1:2:3 title "L=160, one ts " with errorbars 1
pause -1
