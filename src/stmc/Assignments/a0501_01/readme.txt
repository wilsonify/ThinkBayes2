
a0501_01 questions and answers: (see a10.r)
===============================

1. How many recursions and how many sweeps are needed for the
   first tunneling event? 

   388 recursions and 29 297 sweeps.

   What is the acceptance rate during this part of the run?

   With nmucasw=20 the acceptance rate is 20*388/29 297=0.265.

2. Answers to the same questions for ten tunneling events:
   787 recursions and 64 138 sweeps and acceptance rate 0.245.

3. The average acceptance rate during the 10,000 sweeps for
   equilibration is 0.245.

4. 84 tunnelings are found during the measurement period.

-----------------------------------------------------------------------

Then:
=====
 
Move the data file mu2d02q020.dat to a0501_03, run the analysis program
and plot with the gnuplot driver files fln.plt.

Other results:
==============

a01.r with nmucasw=01*nq=02 (instead of 10*nq for a10.r).

Timing on some PC (with and without production):

 nmucasw=01*nq  01m+11s:  787 recursions and (64138+33*10,000) sweeps.
 nmucasw=10*nq    12.9s:  787 recursions and  64138 sweeps.

 nmucasw=10*nq  01m+11s: 7317 recursions and (60241+33*10,000) sweeps.
 nmucasw=10*nq    13.0s: 7317 recursions and  60241 sweeps.
