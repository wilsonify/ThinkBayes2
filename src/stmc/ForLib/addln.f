      FUNCTION ADDLN(ALN,BLN)
C Copyright Bernd Berg, Aug 19 2003.
C Given ln(A) and ln(B), the function returns ln(C) with C=A+B.
      include 'implicit.sta'
      include 'constants.par'
      ADDLN=MAX(ALN,BLN)+LOG(ONE+EXP(-ABS(ALN-BLN)))
      RETURN
      END


