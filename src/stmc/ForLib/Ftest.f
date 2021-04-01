      SUBROUTINE FTEST(EB1,NDAT1,EB2,NDAT2,Q)
C VARIANCE RATIO TEST (F-TEST): COMPARISION OF TWO VARIANCES.
      include 'implicit.sta' 
      include 'constants.par'
      COMMON /PARFT/ NF1,NF2
C DEGREES OF FREEDOM:
      NF1=NDAT1-1
      NF2=NDAT2-1
C F-RATIO:
      VA1=EB1**2*NDAT1
      VA2=EB2**2*NDAT2
      F=VA1/VA2  !  Definitions take per degree of freedom already into account!
      Q=TWO*F_DF(F)
      IF(Q.GT.ONE) Q=TWO-Q
      RETURN
      END
