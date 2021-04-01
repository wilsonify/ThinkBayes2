      FUNCTION ADDLN_CUT(ALN,BLN)
C Copyright Bernd Berg, May 25 2001.
C Given ln(A) and ln(B), the function returns ln(C) with C=A+B.
      include 'implicit.sta'
      include 'constants.par'
      PARAMETER(CUT_OFF=700*ONE) ! Change with precision!
      ABS_LN=ABS(ALN-BLN)
      IF(ABS_LN.GT.CUT_OFF) THEN
        ADDLN_CUT=MAX(ALN,BLN)
      ELSE
        ADDLN_CUT=MAX(ALN,BLN)+LOG(ONE+EXP(-ABS_LN))
      END IF
      RETURN
      END
