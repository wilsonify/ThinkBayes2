#set term pos eps enhanced defaultplex "Helvetica" 26
#set output "df.eps"
set noyzeroaxis
set xlabel "x"
set ylabel "F"
set yrange [0:1]
set key -1.2,0.9
plot 'fig2.d' using 1:2 title "Uniform" with line 1,\
     'fig2.d' using 1:3 title "Gaussian" with line 2,\
     'fig2.d' using 1:4 title "Cauchy" with line 3
pause -1
