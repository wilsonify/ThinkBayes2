      PROGRAM EB0
C
C MEAN VALUES AND ERROR BARS.
C  
      include '../../ForLib/implicit.sta'
      PARAMETER (NDAT=2**10,NDA2=2*NDAT)
      DIMENSION DATA(NDA2)
C
      PRINT*,'NDAT =',NDAT
      CALL RMASET(-1,-1,1,0,'rmarin.d')
      DO I=1,3
        DO N=1,NDAT
          IF(I.EQ.1) CALL RANMAR(DATA(N))
          IF(I.EQ.2) CALL RMAGAU(DATA(N),DATA(NDAT+N))
          IF(I.EQ.3) CALL RMACAU(DATA(N))
        END DO
        IF(I.NE.2) CALL STEB0(NDAT,DATA,XM,XV,XE)
        IF(I.EQ.2) CALL STEB0(NDA2,DATA,XM,XV,XE)
        IF(I.EQ.1) PRINT*,'UNIFORM  RANDOM NUMBERS:'
        IF(I.EQ.2) PRINT*,'GAUSSIAN RANDOM NUMBERS:'
        IF(I.EQ.3) PRINT*,'CAUCHY   RANDOM NUMBERS:'
        WRITE(6,100) XM,XE
      END DO
100   FORMAT(1X,'XM =',1F14.7,'  +/-',1F14.7)
      XM=STMEAN(NDAT,DATA)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmagau.f'
      include '../../ForLib/rmacau.f'
      include '../../ForLib/stmean.f'
      include '../../ForLib/steb0.f'
