
o3_tshb.f     Heat bath  for the O3 (Heisenberg) model.
              Direct, convenient choice of coordinates -> o3_mchb.f.

100 instead of 20 bins:

 on_ts:   em =    -0.8521048614 +/-   0.0000620316
 o3_tshb: em =    -0.8521379479 +/-   0.0000359805

 on_ts on both lnx20 and okamotpc1:
 beta,nlink,nd,nla:   1.1000000000     20000  2    100  100
 nequi,nrpt,nmeas:           10000       100      2500
 actm =   0.4260524307 +/-   0.0000310158
 em =    -0.8521048614 +/-   0.0000620316
 time on okamotopc1: 44m51s.

 o3_tshb on lnx20: 
 beta,nlink,nd,nla:   1.1000000000     20000  2    100  100
 nequi,nrpt,nmeas:           10000       100      2500
 actm =   0.4260689740 +/-   0.0000179902
 em =    -0.8521379479 +/-   0.0000359805

 o3_tshb on okamotopc1:
 beta,nlink,nd,nla:   1.1000000000     20000  2    100  100
 nequi,nrpt,nmeas:           10000       100      2500
 actm =   0.4260459975 +/-   0.0000158742
 em =    -0.8520919949 +/-   0.0000317484
