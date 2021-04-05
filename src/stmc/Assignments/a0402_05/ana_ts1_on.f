      program ana_ts1_on
C Copyright, Bernd Berg, Feb 11 2002. Single time series analysis, 
C           prepares plot. Calculates the average energy (action).
      include '../../ForLib/implicit.sta'
      character cd*1,cn*2,cl*3
C For autocorrelation calculations:
C NT    Maximum time distance considered.
C NSTEP Stepsize in the data file for the plot.
      PARAMETER(NT=2**12,NBINS=32,NSTEP=2**4,LMEAN=.TRUE.)
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      DIMENSION ACORJ(NBINS,0:NT),ACOR(0:NT),WORK(NBINS)
      DIMENSION ACINTJ(NBINS,0:NT),ACINT(0:NT),ACE(0:NT)
      include 'lat.dat'
      dimension ts1a(nrpt*nmeas),act(nrpt),actv(nrpt),acte(nrpt)
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ana_ts1_on: ltest."
C
      ns=nsfun(nla,nd)
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cn,'(I2.2)') n
      write(cl,'(I3.3)') nla(1)
c     open(iud1,file="o"//cn//"_"//cd//"d"//cl//"l0.d",
      open(iud1,file="o"//cn//"_"//cd//"d"//cl//"l2b2.d",
     &          form="unformatted",status="old")
      read(iud1) beta_in,nd_in,nla,nlink,nequi_in,nrpt_in,nmeas_in
      write(iuo,'(/," beta,nlink,nd,nla:",F15.10,I10,I3,2X,4I5)')
     &                beta,nlink,nd_in,nla
      write(iuo,'(" nequi,nrpt,nmeas: ",I15,2I10)')
     &              nequi_in,nrpt_in,nmeas_in
      if(nequi/=nequi_in) stop "nequi.ne.nequi_in."
      if(nrpt/=nrpt_in) stop "nrpt.ne.nrpt_in."
      if(nmeas/=nmeas_in) stop "nmeas.ne.nmeas_in."
      ndat=0
      do irpt=1,nrpt
        read(iud1) tsa,acpt,i_in
        do imeas=1,nmeas
          ndat=ndat+1
          ts1a(ndat)=tsa(imeas)
        end do
        call steb0(nmeas,tsa,act(irpt),actv(irpt),acte(irpt))
      end do
      close(iud1)
      write(iuo,'(" ndat =",i16)') ndat
C
      call steb0(nrpt,act,actm,actmv,actme)
      write(iuo,'(" actm = ",F15.10," +/-",F15.10)') actm,actme
      write(iuo,'(" em =   ",F15.10," +/-",F15.10)') (-2*actm),(2*actme)
      call steb0(nrpt,actv,actvm,actvv,actve)
      write(iuo,'(" actvm =",G15.6," +/-",G15.6)') actvm,actve
      call steb0(nrpt,acte,actem,actev,actee)
      write(iuo,'(" actem =",G15.6," +/-",G15.6)') actem,actee

      DO IT=0,NT
        ACOR(IT)=AUTCORF(IT,ndat,ts1a,LMEAN)
        CALL AUTCORJ(IT,ndat,NBINS,ts1a,WORK,ACORJ(1,IT),LMEAN)
        CALL STEBJ0(NBINS,ACORJ(1,IT),ACM,ACV,ACE(IT))
      END DO
C
c     open(iud1,file="a"//cn//"_"//cd//"d"//cl//"l0ts1.d",
      open(iud1,file="a"//cn//"_"//cd//"d"//cl//"l2b2ts1.d",
     &          form="formatted",status="unknown")
      CALL AC_INT(NT,ACOR,ACINT)
      CALL AC_INTJ(NT,NBINS,ACORJ,ACINTJ)
      DO IT=0,NT,NSTEP
        CALL STEBJ0(NBINS,ACINTJ(1,IT),ACM,ACV,ACE(IT))
        ABIAS=100*ABS(ACINT(IT)-ACM)/ACINT(IT) ! Bias in %.
        WRITE(IUD1,'(I6,2F20.4)') IT,ACINT(IT),ACE(IT)
      END DO
      close(iud1)
      WRITE(IUO,*) "Autocorrelation times are on the a*.d file."
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
