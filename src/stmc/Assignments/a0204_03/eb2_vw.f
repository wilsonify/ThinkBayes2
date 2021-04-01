      PROGRAM EB2_VW
C
C Copyright, Bernd Berg, Oct 30, 2000.
C DISASTER WITH SMALL BIN SIZES. AVERAGES WEIGHTED WITH ONE OVER
C THEIR ESTIMATED VARIANCE. RESULTS FOR MEAN VALUES AND ERROR BARS.
C  
      include '../../ForLib/implicit.sta' 
      PARAMETER (NDAT=19200,KMAX=6,IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      DIMENSION DAT0(NDAT),JDAT(KMAX),DAT2(NDAT),EB(NDAT),WGHT(NDAT)
      DIMENSION XM(0:KMAX),XE(0:KMAX), KDAT(KMAX)
      DATA JDAT /2,3,4,8,16,32/
C
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'nofile')
C
      DO I=1,NDAT
        CALL RANMAR(DAT0(I))
      END DO
      CALL STEB0(NDAT,DAT0,XM(0),XV,XE(0))
C
      DO K=1,KMAX
        DO I=1,NDAT,JDAT(K)
          J=1+I/JDAT(K)
          CALL STEB0(JDAT(K),DAT0(I),DAT2(J),XV,EB(J))
        END DO
        WGHT(1)=-1.0D00
        KDAT(K)=NDAT/JDAT(K)
        CALL STEB2(KDAT(K),DAT2,EB,WGHT,XM(K),XE(K))
      END DO
C
      WRITE(IUO,100) JDAT
      WRITE(IUO,101) NDAT,KDAT
      WRITE(IUO,102) XM
      WRITE(IUO,103) XE
100   FORMAT(1X,'Binsize:',8X,6I8)
101   FORMAT(1X,'Data:   ',7I8)
102   FORMAT(1X,'Means:  ',7F8.4)
103   FORMAT(1X,'Errors: ',7F8.4)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/steb0.f'
      include '../../ForLib/steb2.f'
