      FUNCTION ERROR_F(X)
C BERG MAY 30, 1999.
C RETURNS THE ERROR FUNCTION ERF(X). THE LONG NOTATION ERROR_F IS USED
C TO AVOID CONFLICTS WITH A POSSIBLY EXISTING INTRINSIC FUNCTION ERF. 
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      ERROR_F=SIGN(ONE,X)*GAMMA_P(HALF,X**2) ! Incomplete gamma function.
      RETURN
      END
