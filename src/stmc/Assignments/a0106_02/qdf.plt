 set noyzeroaxis
 set yrange [0:0.525]
 plot "qdf01.d" using 1:3 with line 1,\
      "qdf02.d" using 1:3 with line  2,\
      "qdf03.d" using 1:3 with line  3
 pause -1
