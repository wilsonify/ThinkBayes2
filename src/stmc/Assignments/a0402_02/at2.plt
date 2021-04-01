# set term pos eps enhanced defaultplex "Helvetica" 26
# set output "iat2.eps"
set xrange [0:265]
set yrange [0:260]
set key 60,240
plot "a02_2d160.d" using 1:2 notitle with line 1,\
"a02_2d160.d" using 1:2:3 title "L=160" with errorbars 1,\
"a02_2d080.d" using 1:2 notitle with line 2,\
"a02_2d080.d" using 1:2:3 title "L= 80" with errorbars 2,\
"a02_2d040.d" using 1:2 notitle with line 3,\
"a02_2d040.d" using 1:2:3 title "L= 40" with errorbars 3,\
"a02_2d020.d" using 1:2 notitle with line 4,\
"a02_2d020.d" using 1:2:3 title "L= 20" with errorbars 4,\
"a02_2d010.d" using 1:2 notitle with line 5,\
"a02_2d010.d" using 1:2:3 title "L= 10" with errorbars 5,\
"a02_2d005.d" using 1:2 notitle with line 6,\
"a02_2d005.d" using 1:2:3 title "L=  5" with errorbars 6
pause -1
