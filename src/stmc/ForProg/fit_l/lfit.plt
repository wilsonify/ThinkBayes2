reset
plot 'lfit.d1' using 1:2:3 with errorbar,\
     'lfit.d2' using 1:2 with line 1,\
     'lfit.d2' using 1:3 with line 1,\
     'lfit.d2' using 1:4 with line 1
pause -1
