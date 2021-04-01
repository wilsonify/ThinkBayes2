#set term pos eps enhanced defaultplex "Helvetica" 26
#set output 'es_ferdi.plt'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "e_s"
set xrange [0:0.6]
set yrange [-2:0]
set key 0.50,-0.22
plot 'ferdi.d' using 1:3 title 'Energy per spin e_s' with line 1
pause -1
