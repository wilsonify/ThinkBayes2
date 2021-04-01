      SUBROUTINE STEBJJ1(N,FJJ,FJ,FJMM,FMM,FV,FE)
C                                                                     
C Copyright, Bernd Berg, Jan 10 2000.
C CALCULATION OF JACKKNIFE ESTIMATORS FROM INPUT OF SECOND  
C FJJ(N-1,N)  AND FIRST  FJ(N)  LEVEL JACKKNIFE BINS:
C
C - THE JACKKNIFE SAMPLE OF BIAS CORRECTED MEAN VALUES FJMM(N),
C - THE BIAS CORRECTED MEAN VALUE FMM,
C - THE VARIANCE  FV  AND THE ERROR BAR  FE  FOR  THE 
C   BIAS CORRECTED MEAN VALUE.
C                                                                     
      include '../../ForLib/implicit.sta'
      DIMENSION FJJ(N-1,N),FJ(N),FJMM(N)
      IF(N.LT.3) STOP 'STEBJJ1: N TOO SMALL!'
      N1=N-1
      DO I=1,N
        CALL BIAS(N1,FJJ(1,I),FJ(I),FJMM(I),BIASJM)
      END DO
      CALL STEBJ0(N,FJMM,FMM,FV,FE)
      RETURN
      END
