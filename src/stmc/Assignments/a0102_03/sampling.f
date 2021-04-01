      PROGRAM SAMPLING
C
C Copyright, Bernd Berg, April 1, 2000.
C ILLUSTRATION OF SAMPLING AND ITS CONVERGENCE.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0,KMAX=11,PROB=FOUR/TEN)
C
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
      WRITE(IUO,*) ' '
      WRITE(IUO,*) 'ILLUSTRATION OF SAMPLING'
      WRITE(IUO,*) ' '
      DO K=1,KMAX
        N=2**(2*K-1)
        II=0
        DO IDAT=1,N
          CALL RANMAR(XR)
          IF(XR.LT.PROB) II=II+1
        END DO
        R=(II*ONE)/(N*ONE) ! Needs change for real*4 or real*16.
        ERROR=ABS(R-PROB)
        WRITE(IUO,'(5X,"K,N,R,ERROR:",2I10,2G16.7)') K,N,R,ERROR
      END DO
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
