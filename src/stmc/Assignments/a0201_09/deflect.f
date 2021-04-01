      PROGRAM DEFLECT
C Copyright, Bernd Berg, Oct 30, 2000.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,NDAT=15,Q0cut=0.05D0,P0cut=ONE-Q0cut)
      DIMENSION DAT(NDAT),EB(NDAT),W(NDAT)
      DATA DAT/1.98D0,1.61D0,1.77D0,1.79D0,1.72D0,1.82D0,2.24D0,2.73D0,
     &         1.70d0,2.01D0,1.70D0,1.77D0,1.85D0,1.57D0,1.87D0/
      DATA EB /0.16D0,0.40D0,0.40D0,0.37D0,0.15D0,0.20D0,0.10D0,0.31D0,
     &          0.43d0,0.27D0,0.10D0,0.20D0,0.21D0,0.08D0,0.30D0/
      XM0=1.75D0 ! Theoretical value.
      EB0=0.0D00
C
      LTEST=.TRUE.  ! Prints informational output.
      LTEST=.FALSE.
C
      WRITE(IUO,*) "  "
      WRITE(IUO,*) "Input data and their error bars:"
      WRITE(IUO,*) "  "
      DO I=1,NDAT
        WRITE(IUO,'(1I6,2F8.2)') I,DAT(I),EB(I)
      END DO
C
      N=NDAT+1
1     CONTINUE
        N=N-1
        W(1)=-ONE
        CALL STEB2(N,DAT,EB,W,XM,XE)
        CALL GAUDIF(XM0,EB0,XM,XE,Q)
        WRITE(IUO,*) "  "
        WRITE(IUO,100) N,XM,XE,Q
100     FORMAT(I4," DATA: XM =",F6.3," +/-",F6.3,", Q =",1F6.3,".")
        IF(LTEST) WRITE(IUO,*) "  "
        NM1=N-1
        Pln=LOG(P0cut)/(N*ONE)
        Qcut=ONE-EXP(Pln) ! Qcut for NM1 Gaussian difference tests.
        Qmin=1.D0
        DO I=1,N
          XM1=DAT(I)
          EB1=EB(I)
          DAT(I)=DAT(N)
          EB(I)=EB(N)
          W(1)=-ONE
          CALL STEB2(NM1,DAT,EB,W,XM,XE)
          CALL GAUDIF(XM1,EB1,XM,XE,Q)
          IF(LTEST) WRITE(IUO,'(I10,F10.4,4F8.2)') I,Q,XM1,EB1,XM,XE
          DAT(I)=XM1
          EB(I)=EB1 
          IF(Q.LT.Qmin) THEN
            Qmin=Q
            Imin=I
          END IF
        END DO
      WRITE(IUO,*) " "
      WRITE(IUO,'(3X,"N,Qcut,Qmin =",I3,F8.4,G10.2,":")') N,Qcut,Qmin
      IF(Qmin.LE.Qcut) THEN
        WRITE(IUO,'(3X,"Imin =",I3,", i.e. XM =",F6.3," +/-",F6.3,
     &        " eliminated.")') Imin,DAT(Imin),EB(Imin)
        DAT(Imin)=DAT(N)
        EB(Imin)=EB(N)
      GO TO 1
      ELSE
        WRITE(IUO,'(3X,"Imin =",I3,", i.e. XM =",F6.3," +/-",F6.3,
     &        " no data point eliminated.")') Imin,DAT(Imin),EB(Imin)
      END IF
      WRITE(IUO,*) " "
C
      STOP
      END

      include '../../ForLib/steb2.f'
      include '../../ForLib/gaudif.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gamma_ln.f'
