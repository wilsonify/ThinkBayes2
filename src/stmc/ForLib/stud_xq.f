      FUNCTION STUD_XQ(Q)
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      EXTERNAL STUD_DF
      COMMON /PARSTUD/ NF
C
      IF(Q==HALF)                THEN
      STUD_XQ=ZERO
      RETURN
                                   ENDIF
      X1=ZERO
      X2=ZERO
      IF(Q>HALF)                THEN
1     X2=X2+ONE
      IF(STUD_DF(X2)<=Q) GO TO 1
                                   ELSE
2     X1=X1-ONE
      IF(STUD_DF(X1)>=Q) GO TO 2
                                   ENDIF
C
      STUD_XQ=FI1(STUD_DF,Q,X1,X2)
C
      RETURN
      END
