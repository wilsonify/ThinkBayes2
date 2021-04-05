      FUNCTION BBI1_NK_DF(P)
C Copyright, Berg, October 23 2000.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'binom.com'
      IF(K>N.OR.N<=0.OR.K<0) STOP "BBI1_NK_DF: False N or K."
      IF(P<ZERO .OR. P>ONE) STOP "BBI1_NK_DF: False P input."
C
      IF(P==ZERO) THEN
        BBI1_NK_DF=ZERO
        IF(K==0) BBI1_NK_DF=ONE
        RETURN
      END IF
C
      IF(P==ONE) THEN
        BBI1_NK_DF=ONE
        RETURN
      END IF
C
      XK=ONE*K
      XNMKM1=ONE*(N-K+1)
      BBI1_NK_DF=BETA_I(P,XK,XNMKM1)
C
      RETURN
      END
