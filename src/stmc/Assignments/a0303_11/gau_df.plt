reset
set key left
plot "gau_df.d" using 2:3 with line 1,\
     "gau_df.d" using 2:4 with line 2
pause -1
