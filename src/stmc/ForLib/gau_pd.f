      FUNCTION GAU_PD(X)
C COPYRIGHT BERND BERG, JUN 1, 1999.
C GAUSSIAN PROBABILITY DENSITY FUNCTION.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      GAU_PD=EXP(-HALF*X**2)/SQRT(TWO*PI)
      RETURN
      END
