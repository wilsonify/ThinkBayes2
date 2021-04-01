#set term pos eps enhanced defaultplex "Helvetica" 26
#set output '2d10q_act.eps'
# set title "Put Figure Title Here"
set xlabel "{/Symbol \142}"
set ylabel "actm"
# set xrange [0:0.8]
set yrange [0.0:1.0]
set key 0.50,0.85
plot 'a2d10q020.d' using 1:3:4 title "Action variable q=10" with errorbar 1,\
 'ab2d10q020.d' using 1:3:4 notitle with errorbar 1
pause -1
