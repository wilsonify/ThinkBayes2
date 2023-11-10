      FUNCTION CHI2_DF(CHI2)
C Copyright, Bernd Berg, July 1 1999.
C CHI2 cumulative distribution function.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /CHI2PAR/ NF
      A=HALF*NF
      X=HALF*CHI2
      CHI2_DF=GAMMA_P(A,X)
      RETURN
      END
