
1. K=14:
========

   Compile and run at128_14.f and compare the IT=15 estimate
   of the integrated autocorrelation time with the IT=15
   value obtained with at21.f in assignment a0401_02, the
   at_int21.d data file there.
 
   Here:        tau_int(15) = 3.959 (22)
   at_int21.d:  tau_int(15) = 3.961 (23) 

   Within statistical errors the results are identical.

   The small difference of the mean values is due to the fact
   that the correlations which jump over the 127 boundaries
   (of the 128 bins) are counted in at_int21.d, but not here.

   Note that also a bias would come from the to be subtracted
   mean value, if it would not be known exactly (zero here).

----------------------------

2. K=10:   Now tau_int(15) = 3.809 (84)
======== 

There is still agreement within two error bars, just the 
accuracy has become bad.

High statistics runs: (results in a10.r and a21.r)
 
   Here:        tau_int(15) = 3.8885 (18)  (NRPT=15*128*128)
   at_int21.d:  tau_int(15) = 3.9182 (22)  (128*a0401_02 stat)
