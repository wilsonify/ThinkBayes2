      PROGRAM EB1
C Copyright, Bernd Berg, Apr 4 2000.
C WEIGHTED AVERAGE: MEAN VALUES AND ERROR BARS.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,KMAX=10)
      DIMENSION DATA(KMAX),WGHT(KMAX)
C
      CALL RMASET(-1,-1,1,0,'rmarin.d')
      IDAT=0
      DO K=1,KMAX
        NDAT=2**K
        IF(K.EQ.KMAX) NDAT=2
        IDAT=IDAT+NDAT
        DATA(K)=ZERO
        DO N=1,NDAT
          CALL RANMAR(XR)
          DATA(K)=DATA(K)+XR
        END DO
        WGHT(K)=(NDAT*ONE)
        DATA(K)=DATA(K)/NDAT
        WRITE(IUO,'(" K,NDAT,DATA(K):",2I9,1F12.4)') K,NDAT,DATA(K)
      END DO
      CALL STEB1(KMAX,DATA,WGHT,XM,XE)
      WRITE(IUO,100) XM,XE
100   FORMAT(1X,'UNIFORM RANDOM NUMBERS: XM =',1F14.7,'  +/-',1F14.7)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/steb1.f'
