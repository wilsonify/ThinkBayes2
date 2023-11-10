      FUNCTION GAU_DF(X)
C GAUSSIAN, CUMULATIVE DISTRIBUTION FUNCTION. BERG, JUN 1, 1999.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      XX=X/SQRT(TWO)
      GAU_DF=HALF+HALF*ERROR_F(XX)
      RETURN
      END
