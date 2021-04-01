# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2_large.eps"
set xrange [-950:8400]
set yrange [0:2700]
set key 800,2500
plot "ta02_2d160.d2" using 1:2 notitle with line 1,\
"ta02_2d160.d2" using 1:2:3 title "L=160 ts1" with errorbars 1,\
"ta02_2d160.d" using 1:2 notitle with line 2,\
"ta02_2d160.d" using 1:2:3 title  "L=160 ts " with errorbars 2,\
"ta02_2d080.d2" using 1:2 notitle with line 3,\
"ta02_2d080.d2" using 1:2:3 title "L= 80 ts1" with errorbars 3,\
"ta02_2d080.d" using 1:2 notitle with line 4,\
"ta02_2d080.d" using 1:2:3 title  "L= 80 ts " with errorbars 4,\
"ta02_2d040.d2" using 1:2 notitle with line 5,\
"ta02_2d040.d2" using 1:2:3 title "L= 40 ts1" with errorbars 5,\
"ta02_2d040.d" using 1:2 notitle with line 6,\
"ta02_2d040.d" using 1:2:3 title  "L= 40 ts " with errorbars 6,\
"ta02_2d020.d2" using 1:2 notitle with line 7,\
"ta02_2d020.d2" using 1:2:3 title "L= 20 ts1" with errorbars 7,\
"ta02_2d020.d" using 1:2 notitle with line 8,\
"ta02_2d020.d" using 1:2:3 title  "L= 20 ts " with errorbars 8
pause -1
