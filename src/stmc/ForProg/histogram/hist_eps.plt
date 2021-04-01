set term pos eps enhanced defaultplex "Helvetica" 26
set output "hist.eps"
set xlabel "x^r"
set ylabel "H"
plot "h01.d" using 1:2 title "  100" with line 1,\
     "h02.d" using 1:2 title "10000" with line 2
