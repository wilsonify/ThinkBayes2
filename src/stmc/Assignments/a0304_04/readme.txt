
Energy per spin on CSIT lnx20 (Linux PC):
=========================================

      parameter(beta=1.0d00,H0=ZERO, nhit=0,nequi=10 000)
      parameter(nrpt=32, nmeas=2 000)

on_ts.f:  -1.3219401553 +/- 0.0008660826 -> eqn.  Identical with Sun.
xy_ts0.f: -1.3194840496 +/- 0.0012957039 -> eqn.  Disagrees with Sun.
o2_ts.f:  -1.3204633636 +/- 0.0011289638          Identical with Sun.
xy_ts1.f: -1.3243048934 +/- 0.0007302563           Disagrees with Sun.

The energy values from on_ts.f and o2_ts.f are not identical in all
digits, because differently many random numbers are used in the
accept/reject Metropolis step.

Timing:
======

Mar 1/2 2002 on CSIT lnx20 PC:
 
on_ts.f 501s,  xy_ts0.f 429s,  o2_ts.f 477s,  xy_ts1.f  981s.

----------------

Mar 1/2 2002 on Saclay Sun Wasa:

on_ts.f 323s,  xy_ts0.f 329s,  o2_ts.f 280s,  xy_ts1.f 1,085s.

---------------------------------------------------------

Energy per spin on Saclay Wasa  (Sun workstation):
=================================================

on_ts.f:  Identical with lnx20.
xy_ts0.f: -1.3212741284 +/- 0.0010233129
o2_ts.f:  Identical with lnx20.
xy_ts1.f: Identical with xy_ts0.f.


