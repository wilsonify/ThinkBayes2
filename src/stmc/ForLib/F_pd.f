      FUNCTION F_PD(F)
C Variance ratio probability density.
      include 'implicit.sta'
      include 'constants.par'
      COMMON /PARFT/ NF1,NF2
      F1H=HALF*NF1
      F2H=HALF*NF2
      Y=F1H*F/F2H
      FH=F1H+F2H
      F_PD=F1H*EXP(-GAMMA_LN(F1H)-GAMMA_LN(F2H)+GAMMA_LN(FH))*
     &     Y**(F1H-ONE)*(Y+ONE)**(-FH)/F2H
      RETURN
      END   
