      FUNCTION CHI2PDF_DF(CHI2)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /CHI2PAR/ NF
      A=HALF*NF
      X=HALF*NF*CHI2
      CHI2PDF_DF=GAMMA_P(A,X)
      RETURN
      END
