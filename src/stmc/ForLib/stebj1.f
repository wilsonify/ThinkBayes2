       SUBROUTINE STEBJ1(N,FJ,FMEAN,FJM,FMM,FV,FE)
C                                                                     
C    PURPOSE: CALCULATION OF THE JACKKNIFE ESTIMATORS FJM  AND
C             THE UNBIASED ESTIMATOR FMM FOR  THE MEAN VALUE <F>,
C             JACKKNIFE ESTIMATOR FV FOR THE VARIANCE <F*F>-<F><F>,
C             AND ERROR BAR, DEFINED AS STANDARD DEVIATION OF FJM
C             FE  := SQRT(FMV) = SQRT(FV/N).   
C                                                                     
C    INPUT:
C                                                                     
C    N       NUMBER OF MEASUREMENTS;  N > 1;
C    FJ      VECTOR CONTAINING THE MEASUREMENTS TO BE AVERAGED;    
C    FMEAN   MEAN VALUE FROM COMPLETE STATISTICS.
C
C    OUTPUT:
C
C    FJM     JACKKNIFE  MEAN VALUE;
C    FMM     UNBIASED MEAN VALUES;
C    FV      VARIANCE;            
C    FE      ERROR BAR.
C                                                                     
      include '../../ForLib/implicit.sta'
      DIMENSION FJ(N)
      IF(N.LT.2) STOP 'STEBJ1: N TOO SMALL!'
      CALL STEBJ0(N,FJ,FJM,FV,FE)
      CALL BIAS(N,FJ,FMEAN,FMM,BIASM)
      RETURN
      END
