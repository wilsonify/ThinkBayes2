      PROGRAM GCINTS
C
C GAUSSIAN: CONFIDENCE LIMIT FOR MULTIPLE STANDARD DEVIATIONS.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,N=6)
      DIMENSION P(N),Q(N),NA(N)
C
      I=N
      DO I=1,N
        NA(I)=I
        S=I*ONE
        X=S/SQRT(TWO)
        P(I)=ERROR_F(X)
        Q(I)=HALF*(ONE-P(I))
      END DO
      WRITE(IUO,'("     n",1I7,5I11)') NA
      WRITE(IUO,'("     p",6G11.2)')   P
      WRITE(IUO,'("     q",6G11.2)')   Q
C
      STOP
      END

      INCLUDE '../../ForLib/error_f.f'
      INCLUDE '../../ForLib/gamma_p.f'
      INCLUDE '../../ForLib/gamma_ln.f'
