      FUNCTION BBI_DF(N,K,P)
C Copyright, Berg, October 23 1998.
      include 'implicit.sta'
      include 'constants.par'
      IF(K.GT.N.OR.N.LE.0.OR.K.LT.0) STOP "BBI_DF: False N or K."
      IF(P.LT.ZERO .OR. P.GT.ONE) STOP "BBI_DF: False P input."
C
      IF(P.EQ.ZERO) THEN
        BBI_DF=ONE
        RETURN
      END IF
C
      IF(P.EQ.ONE) THEN
        BBI_DF=ZERO
        IF(K.EQ.N) BBI_DF=ONE
        RETURN
      END IF
C
      XKP1=ONE*(K+1)
      XNMK=ONE*(N-K)
      BBI_DF=ONE-BETA_I(P,XKP1,XNMK)
C
      RETURN
      END
