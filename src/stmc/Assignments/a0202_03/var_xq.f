      PROGRAM var_xq
C
C Variance fractiles: experimental verification.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (IUO=6,IUD=10,NDAT=10000,ND2=2,ISEED1=1,ISEED2=0)
      DIMENSION VAR(NDAT),DATA(ND2)
C
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'nofile')
      WRITE(IUO,*) ' '
      DO N=1,NDAT
        CALL RMAGAU(DATA(1),DATA(2))
        CALL STEB0(ND2,DATA,DM,DV,VAR(N))
        VAR(N)=2.0D00*VAR(N)**2
      END DO
C
      CALL HEAPSORT(NDAT,VAR)
      Q=0.15D00
      CALL QTILES(NDAT,VAR,Q,VQ1,VQ2)
      WRITE(IUO,100) Q,VQ1,VQ2
100   FORMAT(1X,'Q,VQ1,VQ2: ',3F18.8)
C
      STOP
      END

      include '../../ForLib/heapsort.f'
      include '../../ForLib/qtiles.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmagau.f'
      include '../../ForLib/steb0.f'
