      FUNCTION BBI1_NK_XQ(Q)
C Copyright, Berg, October 23 2000.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'binom.com'
      external BBI1_NK_DF
      IF(N.LE.0) STOP "BBI_NK_XQ: N.LE.0."
      IF(K.LT.0.OR.K.GT.N) STOP "BBI_NK_XQ: K out of range."
      IF(K.EQ.0) BBI1_NK_XQ=ZERO
      IF(K.EQ.0) RETURN
      BBI1_NK_XQ=FI1(BBI1_NK_DF,Q,ZERO,ONE)
      RETURN
      END
