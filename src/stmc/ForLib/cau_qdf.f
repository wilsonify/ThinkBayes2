      FUNCTION CAU_QDF(X)
C CAUCHY, CUMULATIVE DISTRIBUTIO FUNCTION. BERG, JUN 1, 1999.
      include 'implicit.sta'
      include 'constants.par'
      CAU_QDF=HALF+ATAN(X)/PI
      IF(CAU_QDF.GT.HALF) GAU_QDF=ONE-GAU_QDF
      RETURN
      END
