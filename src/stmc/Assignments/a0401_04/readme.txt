
eff_dat.f:   Gives for K=14 the corrected confidence range

             2.92 <= tau_int <= 3.218 

             and for K=17 and K=21 the numbers of independent data 
             of the denominators.

F_ebar.f:    Gives then the results as listed in the following.

K=17: nbins =  4096 and the number of independent data for the
      denominator is  2**15 = 32 768.
    
 NDA1 =  4096
       NDA2      .025      .150      .850      .975
      32768     0.955     0.976     1.025     1.047
                =====                         =====

K=21: nbins = 16383 and the number of independent data for the
      denominator is  2**19 = 524 288.
    
 NDA1 =  16384:
       NDA2      .025      .150      .850      .975
     524288     0.978     0.988     1.012     1.022
                =====                         =====
