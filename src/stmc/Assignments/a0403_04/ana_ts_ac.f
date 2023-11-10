      program ana_ts_ac
C Copyright, Bernd Berg, Sep 30 2002. Potts model, time series analysis,
C prepares plots and jackknife data for autocorrelations.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3
C For autocorrelation calculations:
C NT      Maximum time difference considered.
C NSTEP   Step size for the data of the plot.
c     PARAMETER(NT=256,NBINS=32,NSTEP=8,LMEAN=.TRUE.)
      PARAMETER(NT=30,NBINS=32,NSTEP=8,LMEAN=.TRUE.)
      include '../../ForLib/constants.par'
      character CBIN*2
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      DIMENSION tsa(nmeas),ACORJ(0:NBINS,0:NT),WORK(NBINS)
      DIMENSION ACM(0:NT),ACE(0:NT),DATA(nrpt*nmeas)
      include 'lat.dat'
      dimension act(nrpt)
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ana_ts_p: ltest."

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
      idat=0
      do irpt=1,nrpt
        read(iud1) tsa,acpt,i_in
        do imeas=1,nmeas
          idat=idat+1
          DATA(idat)=tsa(imeas)
        end do
        act(irpt)=stmean(nmeas,tsa)
      end do
      close(iud1)

      OPEN(iud1,FILE='acor.d',FORM='formatted',STATUS='unknown')
      WRITE(IUO,*) ' '
      DO IT=0,NT
        ACORJ(0,IT)=AUTCORF(IT,nmeas,DATA,LMEAN)
        CALL AUTCORJ(IT,nmeas,NBINS,DATA,WORK,ACORJ(1,IT),LMEAN)
        CALL STEBJ0(NBINS,ACORJ(1,IT),ACM(IT),ACV,ACE(IT))
        DO IBIN=1,NBINS
          IF(ACORJ(IBIN,IT)-ACE(IT)<=ZERO) THEN
            WRITE(IUO,'("IT,IBIN =",2I9," use smaller NT!")') IT,IBIN
            STOP "ANA_TS_AC: ACORJ consistent with zero."
          END IF
        END DO
        BIAS=ABS(ACORJ(0,IT)-ACM(IT))/ABS(ACORJ(0,IT)) 
        WRITE(iud1,'(1I6,4F14.8)') IT,ACORJ(0,IT),ACM(IT),ACE(IT),BIAS
      END DO
      CLOSE(iud1)

      DO IBIN=0,NBINS
        WRITE(CBIN,'(I2.2)') IBIN
        OPEN(iud1,FILE="datj"//Cbin//".d",FORM='FORMATTED',
     &           STATUS='UNKNOWN')
        IF(IBIN==0) WRITE(iud1,'(2I9,4G16.6)')
     &     NBINS,(NT+1),0.005D00,-0.05D00,0.002D00,-0.1D00 
        DO IT=0,NT
          WRITE(iud1,'(F10.0,2F14.8)') (IT*ONE),ACORJ(IBIN,IT),ACE(IT)
        END DO
        CLOSE(iud1)
      END DO

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
