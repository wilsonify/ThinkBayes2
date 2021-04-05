      FUNCTION BBI_QDF(N,K,P)
C Copyright, Berg, October 23 1999.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      IF(K>N.OR.N<=0.OR.K<0) STOP "BBI_QDF: False N or K."
      IF(P<ZERO .OR. P>ONE) STOP "BBI_QDF: False P input."
C
      IF(P==ZERO.OR.P==ONE) THEN
        BBI_QDF=ZERO
        RETURN
      END IF
C
      XKP1=ONE*(K+1)
      XNMK=ONE*(N-K)
      BBI_QDF=ONE-BETA_I(P,XKP1,XNMK)
      IF(BBI_QDF>HALF) BBI_QDF=ONE-BBI_QDF
C
      RETURN
      END
