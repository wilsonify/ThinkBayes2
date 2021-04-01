      SUBROUTINE STEB0(N,X,XM,XV,XE)
C  COPYRIGHT BERND BERG, FEB 10, 1990.	
C  INPUT:   ARRAY X(N) WITH (GAUSSIAN) DATA.
C  OUTPUT:  MEAN VALUE XM, UNBIASED VARIANCE XV AND ERROR BAR XE.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION X(N)
      IF(N.LT.2) STOP 'STEB0: N HAS TO BE .GE. 2!'
C MEAN VALUE:
      XM=ZERO
      DO I=1,N
        XM=XM+X(I)
      END DO
      XM=XM/N
C ESTIMATOR FOR THE VARIANCE:
      XV=ZERO
      DO I=1,N
        XV=XV+(X(I)-XM)**2
      END DO
      XV=XV/(N-1)
C ERROR BAR:
      XE=SQRT(XV/(N*ONE))
      RETURN
      END
