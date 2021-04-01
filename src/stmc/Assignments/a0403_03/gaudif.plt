reset
set title "Gaussian difference tests"
set yrange[-0.03:1.03]
plot 'gaudif.d' using 1:5 notitle with line 1
pause -1
