      FUNCTION BINO_NK_DF(P)
C Copyright, Berg, October 23 2000.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'binom.com'
      IF(K.GT.N.OR.N.LE.0.OR.K.LT.0) STOP "BINO_NK_DF: False N or K."
      IF(P.LT.ZERO .OR. P.GT.ONE) STOP "BINO_NK_DF: False P input."
C
      IF(P.EQ.ZERO) THEN
        BINO_NK_DF=ONE
        RETURN
      END IF
C
      IF(P.EQ.ONE) THEN
        BINO_NK_DF=ZERO
        IF(K.EQ.N) BINO_NK_DF=ONE
        RETURN
      END IF
C
      I=0
      BINO_NK_DF=BINO_PD(N,I,P)
      DO I=1,K
        BINO_NK_DF=BINO_NK_DF+BINO_PD(N,I,P)
      END DO
C
      RETURN
      END
