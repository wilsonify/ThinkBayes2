
a0501_02 questions and answers: (see a10.r)
===============================

1. How many recursions and how many sweeps are needed for the
   first tunneling event? 

   2 163 recursions and  549 744 sweeps.

   What is the acceptance rate during this part of the run?

   With nmucasw=100 the acceptance rate is 100*2 163/549 744=0.393.

2. Answers to the same questions for ten tunneling events:
   4 269 recursions and 1 211 480 sweeps and acceptance rate 0.352.

3. The average acceptance rate during the 10,000 sweeps for
   equilibration is 0.459.

4. 45 tunnelings are found during the measurement period.

-----------------------------------------------------------------------

Then:
=====
 
Move the data file mu2d10q020.dat to a0501_05, run the analysis program
and plot with the gnuplot driver files fln.plt.

Other results:
==============

a01.r nmucasw=01*nq=10  (instead of nmucasw=10*nq=100 for a10.r)

Timing with and without production on some PC
(shorter production runs on t01.r and t10.r)

 nmucasw=01*nq  03m+20s:  24 142 rec and    825 996 swp.
 nmucasw=01*nq  04m+11s:  24 142 rec and   (825 996 + 33* 10 000) swp.
 nmucasw=01*nq  14m+39s:  24 142 rec and   (825 996 + 33*100 000) swp.

 nmucasw=10*nq  04m+47s:   4 269 rec and  1 211 480 swp.
 nmucasw=10*nq  05m+29s:   4 269 rec and (1 211 480 + 33* 10 000) swp.
 nmucasw=10*nq  15m+59s:   4 269 rec and (1 211 480 + 33*100 000) swp.

