set xlabel "x"
set ylabel "y"
set xrange [-1:10]
set yrange [-1:10]
set arrow from  0,0 to   0.769, 0.221
set arrow from  1,0 to   1.754,-0.267
set arrow from  2,0 to   2.522,-0.607
set arrow from  3,0 to   3.192,-0.777
set arrow from  4,0 to   3.767,-0.765
set arrow from  5,0 to   4.381,-0.507
set arrow from  6,0 to   5.202, 0.055
set arrow from  7,0 to   6.493, 0.619
set arrow from  8,0 to   8.107, 0.793
set arrow from  9,0 to   9.436, 0.671
set arrow from  0,1 to   0.730, 1.327
set arrow from  1,1 to   1.794, 0.904
set arrow from  2,1 to   2.533, 0.404
set arrow from  3,1 to   3.265, 0.245
set arrow from  4,1 to   3.724, 0.249
set arrow from  5,1 to   4.331, 0.562
set arrow from  6,1 to   5.200, 1.022
set arrow from  7,1 to   6.396, 1.525
set arrow from  8,1 to   7.916, 1.796
set arrow from  9,1 to   9.398, 1.694
set arrow from  0,2 to   0.774, 2.202
set arrow from  1,2 to   1.771, 1.788
set arrow from  2,2 to   2.507, 1.381
set arrow from  3,2 to   3.114, 1.208
set arrow from  4,2 to   3.621, 1.295
set arrow from  5,2 to   4.327, 1.568
set arrow from  6,2 to   5.229, 2.214
set arrow from  7,2 to   6.458, 2.588
set arrow from  8,2 to   7.925, 2.797
set arrow from  9,2 to   9.516, 2.611
set arrow from  0,3 to   0.732, 3.323
set arrow from  1,3 to   1.782, 2.832
set arrow from  2,3 to   2.551, 2.420
set arrow from  3,3 to   3.142, 2.213
set arrow from  4,3 to   3.488, 2.385
set arrow from  5,3 to   4.339, 2.550
set arrow from  6,3 to   5.205, 2.914
set arrow from  7,3 to   6.429, 3.561
set arrow from  8,3 to   7.747, 3.759
set arrow from  9,3 to   9.378, 3.705
set arrow from  0,4 to   0.764, 4.236
set arrow from  1,4 to   1.768, 3.777
set arrow from  2,4 to   2.618, 3.492
set arrow from  3,4 to   3.136, 3.212
set arrow from  4,4 to   3.614, 3.299
set arrow from  5,4 to   4.271, 3.670
set arrow from  6,4 to   5.204, 3.918
set arrow from  7,4 to   6.395, 4.523
set arrow from  8,4 to   7.870, 4.789
set arrow from  9,4 to   9.479, 4.641
set arrow from  0,5 to   0.762, 5.244
set arrow from  1,5 to   1.743, 4.705
set arrow from  2,5 to   2.580, 4.449
set arrow from  3,5 to   3.081, 4.204
set arrow from  4,5 to   3.698, 4.259
set arrow from  5,5 to   4.337, 4.553
set arrow from  6,5 to   5.202, 5.049
set arrow from  7,5 to   6.473, 5.602
set arrow from  8,5 to   7.930, 5.797
set arrow from  9,5 to   9.511, 5.615
set arrow from  0,6 to   0.750, 6.278
set arrow from  1,6 to   1.783, 5.837
set arrow from  2,6 to   2.499, 5.375
set arrow from  3,6 to   3.008, 5.200
set arrow from  4,6 to   3.896, 5.207
set arrow from  5,6 to   4.306, 5.602
set arrow from  6,6 to   5.223, 6.189
set arrow from  7,6 to   6.469, 6.599
set arrow from  8,6 to   7.808, 6.777
set arrow from  9,6 to   9.503, 6.622
set arrow from  0,7 to   0.634, 7.488
set arrow from  1,7 to   1.800, 7.015
set arrow from  2,7 to   2.519, 6.391
set arrow from  3,7 to   3.262, 6.244
set arrow from  4,7 to   3.849, 6.214
set arrow from  5,7 to   4.463, 6.407
set arrow from  6,7 to   5.211, 7.133
set arrow from  7,7 to   6.382, 7.508
set arrow from  8,7 to   7.899, 7.794
set arrow from  9,7 to   9.429, 7.675
set arrow from  0,8 to   0.660, 8.452
set arrow from  1,8 to   1.777, 7.810
set arrow from  2,8 to   2.582, 7.451
set arrow from  3,8 to   3.338, 7.275
set arrow from  4,8 to   3.810, 7.223
set arrow from  5,8 to   4.379, 7.496
set arrow from  6,8 to   5.216, 8.158
set arrow from  7,8 to   6.423, 8.554
set arrow from  8,8 to   7.878, 8.791
set arrow from  9,8 to   9.433, 8.673
set arrow from  0,9 to   0.742, 9.299
set arrow from  1,9 to   1.773, 8.796
set arrow from  2,9 to   2.601, 8.472
set arrow from  3,9 to   3.116, 8.208
set arrow from  4,9 to   3.885, 8.208
set arrow from  5,9 to   4.412, 8.458
set arrow from  6,9 to   5.233, 9.229
set arrow from  7,9 to   6.502, 9.626
set arrow from  8,9 to   8.022, 9.800
set arrow from  9,9 to   9.459, 9.655
plot "out_of_range.dat" using 1:2 notitle with line 1
pause -1
