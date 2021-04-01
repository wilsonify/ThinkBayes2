 set noyzeroaxis
 set key    -1.94139    ,0.95
 plot "df01.d" using 1:2 with line 1,\
      "df01.d" using 1:3 with line 1,\
      "df02.d" using 1:2 with line  2,\
      "df02.d" using 1:3 with line  2,\
      "df03.d" using 1:2 with line  3,\
      "df03.d" using 1:3 with line  3
 pause -1
