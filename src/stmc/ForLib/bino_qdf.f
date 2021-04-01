      FUNCTION BINO_QDF(N,K,P)
C Copyright, Berg, October 23 1998.
      include 'implicit.sta'
      include 'constants.par'
      IF(K.GT.N.OR.N.LE.0.OR.K.LT.0) STOP "BINO_QDF: False N or K."
      IF(P.LT.ZERO .OR. P.GT.ONE) STOP "BINO_QDF: False P input."
C
      IF(P.EQ.ZERO.OR.P.EQ.ONE) THEN
        BINO_QDF=ZERO
        RETURN
      END IF
C
      I=0
      BINO_QDF=BINO_PD(N,I,P)
      DO I=1,K
        BINO_QDF=BINO_QDF+BINO_PD(N,I,P)
      END DO
      IF(BINO_QDF.GT.HALF) BINO_QDF=ONE-BINO_QDF
C
      RETURN
      END
