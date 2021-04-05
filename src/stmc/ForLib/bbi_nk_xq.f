      FUNCTION BBI_NK_XQ(Q)
C Copyright, Berg, October 23 2000.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'binom.com'
      external BBI_NK_DF
      IF(N<=0) STOP "BBI_NK_XK: N.LE.0."
      IF(K<0.OR.K>N) STOP "BBI_NK_XK: K out of range."
      IF(K==N) BBI_NK_XQ=ONE
      IF(K==N) RETURN
      BBI_NK_XQ=FI1(BBI_NK_DF,Q,ZERO,ONE)
      RETURN
      END
