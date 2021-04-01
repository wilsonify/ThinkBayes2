      FUNCTION CHI2PDF_QDF(CHI2)
C Coyright, Bernd Berg, July 3 1999.
C CHI2 pdf: peaked distribution function.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /CHI2PAR/ NF
      A=HALF*NF
      X=HALF*NF*CHI2
      CHI2PDF_QDF=GAMMA_P(A,X)
      IF(CHI2PDF_QDF.GT.HALF) CHI2PDF_QDF=ONE-CHI2PDF_QDF
      RETURN
      END
