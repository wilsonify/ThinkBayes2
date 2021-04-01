
Energy per spin on CSIT lnx20 (Linux PC):
=========================================

      parameter(beta=0.5d00,H0=ZERO, nhit=0,nequi=10 000)
      parameter(nrpt=32, nmeas=2 000)

on_ts.f:  -0.5475628782 +/- 0.0001297518 -> eqn.  Identical with Sun.
xy_ts0.f: -0.5475486478 +/- 0.0001297117 -> eqn.  Disagrees with Sun.
o2_ts.f:  -0.5474360236 +/- 0.0001153428 Identical with Sun.
xy_ts1.f: -0.5472388171 +/- 0.0001656051          Disagrees with Sun.

The energy values from on_ts.f and o2_ts.f are not identical in all
digits, because differently many random numbers are used in the
accept/reject Metropolis step.

Timing:
======

Feb 28 2002 on CSIT lnx20 PC:

on_ts.f 514s,  xy_ts0.f 409s,  o2_ts.f 459s,  xy_ts1.f 960s.

Apr 25 2002 on OkamotoPC1

----------------

Feb 26 2002 on Saclay Sun Wasa:

on_ts.f 313s,  xy_ts0.f 336s,  o2_ts.f 260s,  xy_ts1.f 1188s.

---------------------------------------------------------

Energy per spin on Saclay Wasa  (Sun workstation):
=================================================

on_ts.f:  Identical with lnx20.
xy_ts0.f: -0.5473949273 +/- 0.0001231536
o2_ts.f:  Identical with lnx20.
xy_ts1.f: -0.5473949273 +/- 0.0001231536  agrees with xy_ts0.f!
