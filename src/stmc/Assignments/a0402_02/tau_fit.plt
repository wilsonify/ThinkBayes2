set logscale x
set logscale y
plot "test.dat" using 1:3:4 with errorbars 1
pause -1
