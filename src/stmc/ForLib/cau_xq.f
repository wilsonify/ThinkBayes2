      FUNCTION CAU_XQ(Q)
C CAUCHY, CUMULATIVE DISTRIBUTIO FUNCTION. BERG, JUN 1, 1999.
      include 'implicit.sta'
      include 'constants.par'
      QQ=PI*(Q-HALF)
      CAU_XQ=TAN(QQ)
      RETURN
      END
