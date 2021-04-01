      SUBROUTINE STEB1(N,X,W,XM,XE)
C  COPYRIGHT BERND BERG, FEB 12, 1990.
C  INPUT:   ARRAY X (GAUSSIAN) DATA AND WEIGHT FACTORS.
C  OUTPUT:  MEAN VALUE XM, AND ITS ERROR BAR XE.
C           WEIGHT FACTORS NORMALIZED TO ONE.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION X(N),W(N)
C
C NORMALIZATION OF WEIGHT FACTORS:
      WNORM=ZERO
      DO I=1,N
        WNORM=WNORM+W(I)
      END DO
      DO I=1,N
        W(I)=W(I)/WNORM
      END DO
C WEIGHTED MEAN VALUE:
      XM=ZERO
      DO I=1,N
        XM=XM+W(I)*X(I)
      END DO
C UNBIASED ESTIMATOR FOR XE**2 (VARIANCE OF THE MEAN):
      XV=ZERO
      DO I=1,N
        XV=XV+W(I)*(X(I)-XM)**2
      END DO       
      XE=SQRT(XV/((N-1)*ONE))
C
      RETURN
      END
