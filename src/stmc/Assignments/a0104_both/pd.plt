reset
set term pos eps enhanced defaultplex "Helvetica" 21
set output "pd.eps"
set noyzeroaxis
set xlabel "{/Helvetica=26 x}"
set ylabel "{/Helvetica=26 f}"
set yrange [0:0.54]
plot 'fig1.d' using 1:2 title "Uniform" with line 1,\
'fig1.d' using 1:3 title "Gaussian" with line 2,\
'h01.d'  using 1:2 title "Histogram" with line 4,\
'fig1.d' using 1:4 title "Cauchy" with line 3
# pause -1
