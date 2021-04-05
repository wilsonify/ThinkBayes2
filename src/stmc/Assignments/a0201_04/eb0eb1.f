      PROGRAM EB0EB1
C Copyright, Bernd Berg, Apr 5 2000.
C WEIGHTED AVERAGE: MEAN VALUES AND ERROR BARS.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,KMAX=6,NDT0=2**KMAX, NRPT=10 000)
      DIMENSION DAT1(KMAX),WGHT(KMAX),DAT0(NDT0),EB1(NRPT),EB0(NRPT)
      DIMENSION VA1(NRPT),VA0(NRPT)
C
      WRITE(IUO,*) '          '
      WRITE(IUO,*) 'KMAX,NRPT:',KMAX,NRPT
      CALL RMASET(-1,-1,1,0,'ranmar.d')
      DO J=1,NRPT
        IDAT=0
        I=0
        DO K=1,KMAX
          NDAT=2**K
          IF(K==KMAX) NDAT=2
          IDAT=IDAT+NDAT
          DAT1(K)=ZERO
          DO N=1,NDAT
            I=I+1
            CALL RANMAR(XR)
            DAT1(K)=DAT1(K)+XR
            DAT0(I)=XR
          END DO
          WGHT(K)=NDAT*ONE
          DAT1(K)=DAT1(K)/WGHT(K)
        END DO
        IF(I/=NDT0) STOP 'ERROR IN COUNTING I=1,...,NDT0.'
        CALL STEB1(KMAX,DAT1,WGHT,XM1,XE1)
        CALL STEB0(NDT0,DAT0,XM0,XV0,XE0)
        VA1(J)=XE1**2
        VA0(J)=XE0**2
        EB1(J)=XE1
        EB0(J)=XE0
      END DO
C
      WRITE(IUO,101) XM1,XE1
101   FORMAT(1X,'UNIFORM RANDOM NUMBERS: XM1 =',1F14.7,'  +/-',1F14.7)
      WRITE(IUO,100) XM0,XE0
100   FORMAT(1X,'UNIFORM RANDOM NUMBERS: XM0 =',1F14.7,'  +/-',1F14.7)
C
      CALL STEB0(NRPT,EB0,EB0M,XV,XE0)
      CALL STEB0(NRPT,EB1,EB1M,XV,XE1)
      CALL STEB0(NRPT,VA0,XV0M,XV,VE0)
      CALL STEB0(NRPT,VA1,XV1M,XV,VE1)
      WRITE(IUO,*) '          '
      WRITE(IUO,'(9X,"VARIANCE",21X,"ERROR BAR")')
      WRITE(IUO,'(" steb1:",1F10.6," +/-",1F9.6,6X,1F10.6," +/-",
     &                       1F9.6)') XV1M,VE1,EB1M,XE1
      WRITE(IUO,'(" steb0:",1F10.6," +/-",1F9.6,6X,1F10.6," +/-",
     &                       1F9.6)') XV0M,VE0,EB0M,XE0
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/steb0.f'
      include '../../ForLib/steb1.f'
