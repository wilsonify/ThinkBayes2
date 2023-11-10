      FUNCTION F_XQ(Q)
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      EXTERNAL F_DF
      COMMON /PARFT/ NF1,NF2
C
      IF(Q==HALF) THEN
        F_XQ=ONE
        RETURN
      ENDIF
      X2=ONE+ONE/TEN
      IF(Q>HALF) THEN
        X1=ONE-ONE/TEN
1       X2=X2+ONE
        IF(F_DF(X2)<=Q) GO TO 1
      ELSE
        X1=ZERO
      ENDIF
C
      F_XQ=FI1(F_DF,Q,X1,X2)
C
      RETURN
      END
