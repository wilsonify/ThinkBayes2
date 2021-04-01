      FUNCTION BINO1_NK_DF(P)
C Copyright, Berg, October 23 2000.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'binom.com'
      IF(K.GT.N.OR.N.LE.0.OR.K.LT.0) STOP "BINO1_NK_DF: False N or K."
      IF(P.LT.ZERO .OR. P.GT.ONE) STOP "BINO1_NK_DF: False P input."
C
      IF(P.EQ.ZERO) THEN
        BINO1_NK_DF=ZERO
        IF(K.EQ.0) BINO1_NK_DF=ONE
        RETURN
      END IF
C
      IF(P.EQ.ONE) THEN
        BINO1_NK_DF=ONE
        RETURN
      END IF
C
      BINO1_NK_DF=BINO_PD(N,K,P)
      KP1=K+1
      DO I=KP1,N
        BINO1_NK_DF=BINO1_NK_DF+BINO_PD(N,I,P)
      END DO
C
      RETURN
      END
