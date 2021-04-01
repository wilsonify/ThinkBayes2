
test_o3.f     Test version of the heat bath, because of problems
              on ile-i (compiler bug!?).

              Cross compilation: Run of the lnx20 executable 
              reproduces the lnx20 result. Hence differences
              are due to different compilations.

o3_tshb.f     Heat bath  for the O3 (Heisenberg) model.
              Direct, convenient choice of coordinates -> o3_mchb.f.

Energy values:
==============

 Linux PC:
 on_ts:   em =  -0.8523023408 +/- 0.0001318595
 o3_tshb: em =  -0.8521226486 +/- 0.0000954768 lnx20
 o3_tshb: em =  -0.8519921589 +/- 0.0000746466 okamotopc1

 Sun workstation:
 on_ts:   em =  -0.8523023408 +/- 0.0001318595 (identical)
 o3_tshb: em =  -0.8519761744 +/- 0.0000782520

Timing:
======

on_ts.f    1703s on lnx20 PC i    at CSIT Feb 20 2002.
on_ts.f     624s on okamotopc1 PC at IMS  Apr 11 2002.
on_ts.f     440s on on uberg laptop       Jul 22 2004.
o3_tshb.f  2305s on lnx20 PC      at CSIT May  6 2002.
o3_tshb.f  1250s on ile-i PC      at IMS  May 06 2002. 

on_ts.f    1109s on Sun wasa at Saclay Feb 20 2002.
o3_tshb.f  1610s on Sun wasa at Saclay Feb 20 2002.
