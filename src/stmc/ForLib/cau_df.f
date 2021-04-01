      FUNCTION CAU_DF(X)
C CAUCHY, CUMULATIVE DISTRIBUTIO FUNCTION. BERG, JUN 1, 1999.
      include 'implicit.sta'
      include 'constants.par'
      CAU_DF=HALF+ATAN(X)/PI
      RETURN
      END
