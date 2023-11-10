      program ana_ts1_p
C Copyright, Bernd Berg, Feb 11 2002. Potts model, single time series 
C analysis, prepares plot. Calculates the average energy (action).
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3
C For autocorrelation calculations:
C NT    Maximum time distance considered.
C NSTEP Stepsize in the data file for the plot.
      PARAMETER(NT=2**13,NBINS=32,NSTEP=2**7,LMEAN=.TRUE.)
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      DIMENSION ACORJ(NBINS,0:NT),ACOR(0:NT),WORK(NBINS)
      DIMENSION ACINTJ(NBINS,0:NT),ACINT(0:NT),ACE(0:NT)
      include 'lat.dat'
      dimension tsa(nrpt*nmeas),tsa_in(nmeas),act(nrpt)
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ana_ts1_p: ltest."
C
      ns=nsfun(nla,nd)
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cq//"_"//cd//"d"//cl//".d",
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
        read(iud1) tsa_in,acpt,i_in
        do imeas=1,nmeas
          ndat=ndat+1
          tsa(ndat)=tsa_in(imeas)
        end do
        act(irpt)=stmean(nmeas,tsa_in)
      end do
      close(iud1)
      write(iuo,'(" ndat =",i16)') ndat
C
      call steb0(nrpt,act,actm,actv,acte)
      write(iuo,'(" actm =",F15.10," +/-",F15.10)') actm,acte
      write(iuo,'(" em =  ",F15.10," +/-",F15.10)') (-2*actm),(2*acte)

      DO IT=0,NT
        ACOR(IT)=AUTCORF(IT,ndat,tsa,LMEAN)
        CALL AUTCORJ(IT,ndat,NBINS,tsa,WORK,ACORJ(1,IT),LMEAN)
        CALL STEBJ0(NBINS,ACORJ(1,IT),ACM,ACV,ACE(IT))
      END DO
C
      open(iud1,file="ta"//cq//"_"//cd//"d"//cl//".d2",
     &          form="formatted",status="unknown")
      CALL AC_INT(NT,ACOR,ACINT)
      CALL AC_INTJ(NT,NBINS,ACORJ,ACINTJ)
      DO IT=0,NT,NSTEP
        CALL STEBJ0(NBINS,ACINTJ(1,IT),ACM,ACV,ACE(IT))
        ABIAS=100*ABS(ACINT(IT)-ACM)/ACINT(IT) ! Bias in %.
        WRITE(IUD1,'(I6,2F20.4)') IT,ACINT(IT),ACE(IT)
      END DO
      close(iud1)
      WRITE(IUO,*) "Autocorrelation times are on the a*.d2 file."
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
