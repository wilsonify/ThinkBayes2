      FUNCTION BINO_QDF(N,K,P)
C Copyright, Berg, October 23 1998.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      IF(K>N.OR.N<=0.OR.K<0) STOP "BINO_QDF: False N or K."
      IF(P<ZERO .OR. P>ONE) STOP "BINO_QDF: False P input."
C
      IF(P==ZERO.OR.P==ONE) THEN
        BINO_QDF=ZERO
        RETURN
      END IF
C
      I=0
      BINO_QDF=BINO_PD(N,I,P)
      DO I=1,K
        BINO_QDF=BINO_QDF+BINO_PD(N,I,P)
      END DO
      IF(BINO_QDF>HALF) BINO_QDF=ONE-BINO_QDF
C
      RETURN
      END
