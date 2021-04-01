      SUBROUTINE STEBJ0(N,FJ,FM,FV,FE)
C                                                                     
C CALCULATION OF JACKKNIFE ESTIMATORS FROM  N  JACKKNIFE BINS  FJ(N):
C                                                                     
C - THE MEAN VALUE:   FM,
C - THE VARIANCE:     FV,
C - THE ERROR BAR:    FE=SQRT(FV/N), (STANDARD DEVIATION OF FM).   
C                                                                     
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION FJ(N)
C
      IF(N.LT.2) STOP 'STEBJ0: N TOO SMALL!'
      FM=ZERO
      FV=ZERO
C
      DO I=1,N
        FM=FM+FJ(I)
      END DO
      FM=FM/N
C 
      DO I=1,N
        FV=FV+(FJ(I)-FM)**2
      END DO
      FV=FV*(N-1)
      FE=SQRT(FV/(N*ONE))
      RETURN
      END
