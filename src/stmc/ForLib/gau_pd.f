      FUNCTION GAU_PD(X)
C COPYRIGHT BERND BERG, JUN 1, 1999.
C GAUSSIAN PROBABILITY DENSITY FUNCTION.
      include 'implicit.sta'
      include 'constants.par'
      GAU_PD=EXP(-HALF*X**2)/SQRT(TWO*PI)
      RETURN
      END
