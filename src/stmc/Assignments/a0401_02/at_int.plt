set term pos eps enhanced defaultplex "Helvetica" 26
set output 'at_int.eps'
# set title "Put Figure Title Here"
set xrange [-1.0:128.0]
set yrange [0.90:4.1]
set xlabel "t"
set ylabel "{/Symbol t}_{int}"
set label "1" at 24,1.1
set label "{/Symbol -}" at 28,1.1
set label "2" at 24,1.9
set label "{/Symbol -}" at 28,1.9
set label "3" at 24,2.7
set label "{/Symbol -}" at 28,2.7
set label "4" at 24,3.5
set label "{/Symbol -}" at 28,3.5
set label "10" at 57.7,3.67
set label "|" at 60,3.5
set label "20" at 87.7,3.67
set label "|" at 90,3.5
set label "30" at 117.7,3.67
set label "|" at 120,3.5
set label "1" at 69,1.2
set label "{/Symbol -}" at 73,1.2
set label "2" at 69,1.7
set label "{/Symbol -}" at 73,1.7
set label "3" at 69,2.2
set label "{/Symbol -}" at 73,2.2
set label "4" at 69,2.7
set label "{/Symbol -}" at 73,2.7
set label "5" at 88.1,1.40
set label "|" at 90,1.2
set label "10" at 102.7,1.40
set label "|" at 105,1.2
set label "15" at 117.7,1.40
set label "|" at 120,1.2
plot "at_int21.d" using 1:2 notitle with line 1,\
     "at_int21.d" using 1:2:4 notitle with errorbar 1,\
     "uat_int21.d" using 1:2 notitle with line 2,\
     "uat_int21.d" using 1:2:4 notitle with errorbar 2,\
     "b_aint21.d" using 2:4 notitle with line 3,\
     "b_aint21.d" using 2:7 notitle with line 3,\
     "b_aint21.d" using 2:8 notitle with line 3,\
     "at_int17a.d" using 1:2 notitle with line 1,\
     "at_int17a.d" using 1:2:4 notitle with errorbar 1,\
     "b_aint17a.d" using 2:4 notitle with line 3,\
     "b_aint17a.d" using 2:5 notitle with line 3,\
     "b_aint17a.d" using 2:6 notitle with line 3,\
     "box1.dat" using 1:2 notitle with line 1,\
     "at_int14a.d" using 1:2 notitle with line 1,\
     "at_int14a.d" using 1:2:4 notitle with errorbar 1,\
     "b_aint14a.d" using 2:4 notitle with line 3,\
     "b_aint14a.d" using 2:5 notitle with line 3,\
     "b_aint14a.d" using 2:6 notitle with line 3,\
     "box2.dat" using 1:2 notitle with line 1
# pause -1
