      FUNCTION CAU_DF(X)
C CAUCHY, CUMULATIVE DISTRIBUTIO FUNCTION. BERG, JUN 1, 1999.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      CAU_DF=HALF+ATAN(X)/PI
      RETURN
      END
