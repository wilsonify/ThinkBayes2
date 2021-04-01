set noyzeroaxis
set key    -1.9,0.94
set xrange[-3.3:3.3]
set yrange[0:0.52]
plot "df01.d" using 1:3 with line 1,\
     "dfexact.d" using 1:3 with line 2
pause -1
