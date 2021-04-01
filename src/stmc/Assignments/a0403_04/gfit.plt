reset
plot 'data.d' using 1:2:3 with errorbar,\
     'gfit.d' using 1:2 with line 1
pause -1
