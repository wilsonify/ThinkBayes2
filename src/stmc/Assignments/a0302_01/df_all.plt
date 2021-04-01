set noyzeroaxis
set key    -1.9,0.94
plot "df01.d" using 1:2 with line 1,\
     "df01.d" using 1:3 with line 1,\
     "dfexact.d" using 1:2 with line 2,\
     "dfexact.d" using 1:3 with line 2
pause -1
