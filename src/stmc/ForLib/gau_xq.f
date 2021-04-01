      FUNCTION GAU_XQ(Q)
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      EXTERNAL GAU_DF
C
      IF(Q.EQ.HALF)                  THEN
      GAU_XQ=ZERO
      RETURN
                                     ENDIF
      X1=ZERO
      X2=ZERO
      IF(Q.GT.HALF)                  THEN
1     X2=X2+ONE
      IF(GAU_DF(X2).LE.Q) GO TO 1
                                     ELSE
2     X1=X1-ONE
      IF(GAU_DF(X1).GE.Q) GO TO 2
                                     ENDIF
      GAU_XQ=FI1(GAU_DF,Q,X1,X2)
C
      RETURN
      END
