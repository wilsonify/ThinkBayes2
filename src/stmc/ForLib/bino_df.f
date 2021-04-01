      FUNCTION BINO_DF(N,K,P)
C Copyright, Berg, October 23 1998.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      IF(K.GT.N.OR.N.LE.0.OR.K.LT.0) STOP "BINO_DF: False N or K."
      IF(P.LT.ZERO .OR. P.GT.ONE) STOP "BINO_DF: False P input."
C
      IF(P.EQ.ZERO) THEN
        BINO_DF=ONE
        RETURN
      END IF
C
      IF(P.EQ.ONE) THEN
        BINO_DF=ZERO
        IF(K.EQ.N) BINO_DF=ONE
        RETURN
      END IF
C
      I=0
      BINO_DF=BINO_PD(N,I,P)
      DO I=1,K
        BINO_DF=BINO_DF+BINO_PD(N,I,P)
      END DO
C
      RETURN
      END
