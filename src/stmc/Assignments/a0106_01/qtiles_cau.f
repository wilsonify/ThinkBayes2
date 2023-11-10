      PROGRAM QTILES_CAU
C
C SAMPLE ESTIMATES FOR THE MEDIAN OF CAUCHY RANDOM NUMBERS.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER (KMAX=8,MMAX=4**KMAX,NMAX=2*MMAX-1)
      DIMENSION DATA(NMAX)
C
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*) ' VARIOUS QTILES FOR CAUCHY RANDOM NUMBERS:'
      WRITE(IUO,*) ' '
      WRITE(IUO,*) '  K       N     X_0.5    X_0.25    X_0.75',
     &                                  '    X_0.15    X_0.85'
      WRITE(IUO,*) ' '
      DO K=2,KMAX
        M=4**K
        N=2*M-1
        DO I=1,N
          CALL RMACAU(DATA(I))
        END DO
        CALL HEAPSORT(N,DATA)
        CALL QTILES(N,DATA,HALF,X05,XHALF)
        IF(X05/=XHALF .OR. X05/=DATA(M)) STOP "MEDIANS DIFFER."
        CALL QTILES(N,DATA,P25,X025,X075)
        CALL QTILES(N,DATA,P15,X015,X085)
        WRITE(IUO,'(1I4,1I8,5F10.4)') K,N,X05,X025,X075,X015,X085
      END DO
      WRITE(IUO,*) 'MEDIAN DEFINITIONS AGREE.'
C
      STOP
      END

      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmacau.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/heapsort.f'
      INCLUDE '../../ForLib/qtiles.f'
