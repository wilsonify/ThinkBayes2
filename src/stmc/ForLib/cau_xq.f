      FUNCTION CAU_XQ(Q)
C CAUCHY, CUMULATIVE DISTRIBUTIO FUNCTION. BERG, JUN 1, 1999.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      QQ=PI*(Q-HALF)
      CAU_XQ=TAN(QQ)
      RETURN
      END
