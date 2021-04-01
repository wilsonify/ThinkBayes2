# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2_large.eps"
# set xrange [-950:8400]
# set yrange [0:2700]
set xrange [0:520]
set yrange [0:42]
set key 300,15
plot "ta02_2d010.d2" using 1:2 notitle with line 1,\
"ta02_2d010.d2" using 1:2:3 title  "L= 10 ts " with errorbars 1,\
"a02_2d010.d" using 1:2 notitle with line 2,\
"a02_2d010.d" using 1:2:3 title  "L= 10 ts " with errorbars 2,\
"a02_2d010m.d" using 1:2 notitle with line 3,\
"a02_2d010m.d" using 1:2:3 title  "L= 10 ts " with errorbars 3,\
"a02_2d010r.d" using 1:2 notitle with line 4,\
"a02_2d010r.d" using 1:2:3 title  "L= 10 ts " with errorbars 4
pause -1
