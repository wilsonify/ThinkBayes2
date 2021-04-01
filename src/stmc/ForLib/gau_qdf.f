      FUNCTION GAU_QDF(X)
C GAUSSIAN, PEAKED DISTRIBUTION FUNCTION. BERG, JUN 1 1999.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      XX=X/SQRT(TWO)
      GAU_QDF=HALF+HALF*ERROR_F(XX)
      IF(GAU_QDF.GT.HALF) GAU_QDF=ONE-GAU_QDF
      RETURN
      END
