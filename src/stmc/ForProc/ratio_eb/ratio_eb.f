      PROGRAM RATIO_EB
C BERG, APR 21, 2002. Ratio of two numbers with error bars in
C                     a conservative Gaussian approximation.
C INPUT:   TWO GAUSSIAN DATA POINTS, MEAN VALUES AND ERROR BARS.
C OUTPUT:  RATIO WITH ERROR BARS.
      PARAMETER(IUO=6)
      include '../../ForLib/implicit.sta'
C
      PRINT*,'RATIO OF 2 MEANS WITH ERROR BARS.'
      PRINT*,'INPUT:   TWO MEANS VALUES, THEIR ERROR BARS 
     & (NOT VARIANCES!)'
      PRINT*,'          '
1     CONTINUE
      PRINT*,'ENTER 1. MEAN VALUE (DATA POINT):'
      READ(5,*) XM1
      PRINT*,'ENTER ERROR BAR OF 1. DATA POINT:'
      READ(5,*) EB1
      PRINT*,'ENTER 2. MEAN VALUE (DATA POINT):'
      READ(5,*) XM2
      PRINT*,'ENTER ERROR BAR OF 2. DATA POINT:'
      READ(5,*) EB2
C
      CALL STEB_RAT(XM1,EB1,XM2,EB2,RATM,RATE,Resmall)
      WRITE(IUO,'(" RATE =",G16.7," +/-",G16.7)') RATM,RATE       
      WRITE(IUO,'(" REsmall:",14X," +/-",G16.7)')  REsmall
C
      STOP
      END

      include '../../ForLib/steb_rat.f'
