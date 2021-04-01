      PROGRAM EB2
C Copyright, Bernd Berg, Oct 10 1999.
C VARIANCE-WEIGHTED AVERAGE: MEAN VALUES AND ERROR BARS.
C  
      include '../../ForLib/implicit.sta' 
      include '../../ForLib/constants.par' 
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(KMAX=10,NMAX=2**(KMAX-1))
      DIMENSION DATA(KMAX),EB(KMAX),WGHT(KMAX),DAT1(NMAX)
C
      CALL RMASET(-IUO,IUD,ISEED1,ISEED2,'ranmar.d')
      WGHT(1)=-ONE
      DO K=1,KMAX
        NDAT=2**K
        IF(K.EQ.KMAX) NDAT=2
        DO N=1,NDAT
          CALL RANMAR(DAT1(N))
        END DO
        CALL STEB0(NDAT,DAT1,DATA(K),XV,EB(K))
      END DO
      CALL STEB2(KMAX,DATA,EB,WGHT,XM,XE)
      WRITE(IUO,100) XM,XE
100   FORMAT(1X,'UNIFORM RANDOM NUMBERS: XM =',1F14.7,'  +/-',1F14.7)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/steb0.f'
      include '../../ForLib/steb2.f'
