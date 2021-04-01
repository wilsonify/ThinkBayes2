      FUNCTION STMEAN(N,X)
C COPYRIGHT BERND BERG, FEB 10 1990.
C CALCULATION OF THE MEAN VALUE FOR N DATA IN X(N).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION X(N)
      XM=ZERO
      DO I=1,N
        XM=XM+X(I)
      END DO
      STMEAN=XM/N
      RETURN
      END
