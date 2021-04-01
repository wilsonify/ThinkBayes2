# set term pos eps enhanced defaultplex "Helvetica" 26
# set output 'pt_hist.eps'
set xrange [0.24:1.0]
set yrange [0:3.2E06]
set xlabel "act"
set ylabel "Histograms"
set key 0.72,3.0E06
plot "histo_1.d" using 1:2 title "{/Symbol b}_0=0.65000" with line 1,\
     "histo_1.d" using 1:3 title "{/Symbol b}_1=0.67746" with line 2,\
     "histo_1.d" using 1:4 title "{/Symbol b}_2=0.69721" with line 3,\
     "histo_1.d" using 1:5 title "{/Symbol b}_3=0.70321" with line 4,\
     "histo_2.d" using 1:2 title "{/Symbol b}_4=0.70734" with line 5,\
     "histo_2.d" using 1:3 title "{/Symbol b}_5=0.71332" with line 6,\
     "histo_2.d" using 1:4 title "{/Symbol b}_6=0.72619" with line 7,\
     "histo_2.d" using 1:5 title "{/Symbol b}_7=0.75000" with line 8
# pause -1

