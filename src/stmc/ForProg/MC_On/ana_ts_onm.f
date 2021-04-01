      program ana_ts_onm
C Copyright, Bernd Berg, Jan 23 2004. On models, time series analysis.
C Calculates the average energy (action), prepares histogram plots.
      include '../../ForLib/implicit.sta'
      character cd*1,cn*2,cl*3
C For autocorrelation calculations:
      PARAMETER(NT=16,NT_est=01,NSTEP=1,NBINS=32,LMEAN=.TRUE.)
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      DIMENSION ACORJ(NBINS,0:NT),ACOR(0:NT),WORK(NBINS)
      DIMENSION ACINTJ(NBINS,0:NT),ACINT(0:NT),ACE(0:NT),ATAU(NRPT,0:NT)
      DIMENSION ACT(NRPT),ACTV0(NRPT,2),ACTV0m(2),ACTV0e(2),RATIO(NRPT)
      DIMENSION TAU_INT(NRPT,2),ACTV_TI(NRPT,2)
      include 'lat.dat'
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ana_ts_onm: ltest."
C
      ns=nsfun(nla,nd)
      WRITE(cd,'(I1.1)') nd ! Prepare to READ the data.
      WRITE(cn,'(I2.2)') n
      WRITE(cl,'(I3.3)') nla(1)
C
      DO ICASE=1,2
        WRITE(IUO,'(/," ICASE = ",I6)') ICASE
        if(ICASE.eq.1) OPEN(IUD1,file="o"//cn//"_"//cd//"d"//cl//".d",
     &                 form="unformatted",status="old")
        if(ICASE.eq.2) OPEN(IUD1,file="o"//cn//"_"//cd//"d"//cl//"m.d"
     &                ,form="unformatted",status="old")
        READ(IUD1) beta_in,nd_in,nla,nlink,nequi_in,NRPT_in,nmeas_in
        WRITE(iuo,'(" beta,nlink,nd,nla:",F15.10,I10,I3,2X,4I5)')
     &                beta,nlink,nd_in,nla
        WRITE(iuo,'(" nequi,NRPT,nmeas: ",I15,2I10)')
     &                nequi_in,NRPT_in,nmeas_in
        if(nequi.ne.nequi_in) stop "nequi.ne.nequi_in."
        if(NRPT.ne.NRPT_in) stop "NRPT.ne.NRPT_in."
        if(nmeas.ne.nmeas_in) stop "nmeas.ne.nmeas_in."
        DO IRPT=1,NRPT
          READ(IUD1) TSA,acpt,i_in
          CALL STEB0(nmeas,TSA,act(IRPT),ACTV0(IRPT,ICASE),acte)
          DO IT=0,NT
            ACOR(IT)=AUTCORF(IT,nmeas,TSA,LMEAN)
            CALL AUTCORJ(IT,nmeas,NBINS,TSA,WORK,ACORJ(1,IT),LMEAN)
            CALL STEBJ0(NBINS,ACORJ(1,IT),ACM,ACV,ACE(IT))
          END DO
C
          CALL AC_INT(NT,ACOR,ACINT)
          CALL AC_INTJ(NT,NBINS,ACORJ,ACINTJ)
          DO IT=0,NT
            CALL STEBJ0(NBINS,ACINTJ(1,IT),ACM,ACV,ACE(IT))
            ABIAS=100*ABS(ACINT(IT)-ACM)/ACINT(IT) ! Bias in %.
            ATAU(IRPT,IT)=ACINT(IT) ! The less biased estimator.
          END DO
C Product of naive variance and autocorrelation time:
          ACTV_TI(IRPT,ICASE)=ACTV0(IRPT,ICASE)*ACINT(NT_est) 
          TAU_INT(IRPT,ICASE)=ACINT(NT_est)
C
        END DO
        CLOSE(IUD1)
C
        CALL STEB0(NRPT,act,actm,av,acte)
        WRITE(iuo,'(" actm  = ",F15.10," +/-",F15.10)') actm,acte
        WRITE(iuo,'(" em =    ",F15.10," +/-",F15.10)') 
     &                                          (-2*actm),(2*acte)
        CALL STEB0(NRPT,ACTV0(1,ICASE),ACTV0m(ICASE),av,ACTV0e(ICASE))
        WRITE(IUO,'(" ACTV0m = ",G15.6," +/-",G15.6)') 
     &                ACTV0m(ICASE),ACTV0e(ICASE)
C
        WRITE(IUO,*) "Autocorrelation times are on the a*.d file."
        if(ICASE.eq.1) OPEN(IUD1,file="a"//cn//"_"//cd//"d"//cl//".d",
     &                     form="formatted",status="unknown")
        if(ICASE.eq.2) OPEN(IUD1,file="a"//cn//"_"//cd//"d"//cl//"m.d",
     &                     form="formatted",status="unknown")
        DO IT=0,NT,NSTEP
          CALL STEB0(NRPT,ATAU(1,IT),ACINT(IT),ACV,ACE(IT))
          ACIV=ns*ACINT(IT)
          ACEV=ns*ACE(IT)
          WRITE(IUD1,'(I6,4F16.4)') IT,ACINT(IT),ACE(IT),ACIV,ACEV
        END DO
        CLOSE(IUD1)
C
      END DO
C
C Integrated autocorrelation times and improvement ratio:
      DO IRPT=1,NRPT
        RATIO(IRPT)=ACTV_TI(IRPT,1)/ACTV_TI(IRPT,2)
      END DO 
      CALL STEB0(NRPT,RATIO,ratm,av,rate)
      CALL STEB0(NRPT,TAU_INT(1,1),TAU_I1,AV1,TAU_I1E)
      CALL STEB0(NRPT,TAU_INT(1,2),TAU_I2,AV2,TAU_I2E)
      WRITE(iuo,'(/," ratm     =",F15.10," +/-",F15.10,/)') ratm,rate
      WRITE(iuo,'(" tau_int1 =",F15.10," +/-",F15.10)') TAU_I1,TAU_I1E
      WRITE(iuo,'(" tau_int2 =",F15.10," +/-",F15.10)') TAU_I2,TAU_I2E
C
      stop
      end

      INCLUDE '../../ForLib/ac_int.f'
      INCLUDE '../../ForLib/ac_intj.f'
      INCLUDE '../../ForLib/autcorf.f'
      INCLUDE '../../ForLib/autcorj.f'
      INCLUDE '../../ForLib/bias.f'
      INCLUDE '../../ForLib/bining.f'
      INCLUDE '../../ForLib/datjack.f'
      INCLUDE '../../ForLib/nsfun.f'
      INCLUDE '../../ForLib/steb0.f'
      INCLUDE '../../ForLib/stebj0.f'
      INCLUDE '../../ForLib/stmean.f'
