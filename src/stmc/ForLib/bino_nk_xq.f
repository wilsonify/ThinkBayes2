      FUNCTION BINO_NK_XQ(Q)
C Copyright, Berg, October 23 2000.
      include 'implicit.sta'
      include 'constants.par'
      include 'binom.com'
      external BINO_NK_DF
      IF(N.LE.0) STOP "BBI_NK_XK: N.LE.0."
      IF(K.LT.0.OR.K.GT.N) STOP "BBI_NK_XK: K out of range."
      IF(K.EQ.N) BINO_NK_XQ=ONE 
      IF(K.EQ.N) RETURN
      BINO_NK_XQ=FI1(BINO_NK_DF,Q,ZERO,ONE)
      RETURN
      END
