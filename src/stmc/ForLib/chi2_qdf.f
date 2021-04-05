      FUNCTION CHI2_QDF(CHI2)
C Copyright, Bernd Berg, July 2 1999.
C CHI2 peaked distribution function.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /CHI2PAR/ NF
      A=HALF*NF
      X=HALF*CHI2
      CHI2_QDF=GAMMA_P(A,X)
      IF(CHI2_QDF>HALF) CHI2_QDF=ONE-CHI2_QDF
      RETURN
      END
