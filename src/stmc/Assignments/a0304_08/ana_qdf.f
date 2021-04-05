      program ana_qdf ! Adaption of older version of ana_ts_on.f.
C Copyright, Bernd Berg, Feb 10 2002. O(n) models, time series analysis,
C prepares plots. Calculates the average energy (action).
      include '../../ForLib/implicit.sta'
      character cd*1,cn*2,cl*3
C For autocorrelation calculations:
C NT      Maximum time difference considered.
C NSTEP   Step size for the data of the plot.
c     PARAMETER(LAUTO=.TRUE.,NT=8192,NBINS=32,NSTEP=64,LMEAN=.TRUE.)
      PARAMETER(LAUTO=.FALSE.,NT=8192,NBINS=32,NSTEP=64,LMEAN=.TRUE.)
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      DIMENSION tsma(nmeas),ACORJ(NBINS,0:NT),ACOR(0:NT),WORK(NBINS)
      DIMENSION ACINTJ(NBINS,0:NT),ACINT(0:NT),ACE(0:NT),ATAU(NRPT,0:NT)
      include 'lat.dat'
      dimension act(nrpt)
      data tsma/nmeas*zero/
      ltest=.true.
      ltest=.false.
C
      ns=nsfun(nla,nd)
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cn,'(I2.2)') n
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="o"//cn//"_"//cd//"d"//cl//".d",
     &          form="unformatted",status="old")
      read(iud1) beta_in,nd_in,nla,nlink,nequi_in,nrpt_in,nmeas_in
      write(iuo,'(/," beta,nlink,nd,nla:",F15.10,I10,I3,2X,4I5)')
     &                beta,nlink,nd_in,nla
      write(iuo,'(" nequi,nrpt,nmeas: ",I15,2I10)')
     &              nequi_in,nrpt_in,nmeas_in
      if(nequi/=nequi_in) stop "nequi.ne.nequi_in."
      if(nrpt/=nrpt_in) stop "nrpt.ne.nrpt_in."
      if(nmeas/=nmeas_in) stop "nmeas.ne.nmeas_in."
      do irpt=1,nrpt
        read(iud1) tsa,acpt,i_in
        act(irpt)=stmean(nmeas,tsa)
        IF(LAUTO) THEN
          DO IT=0,NT
            ACOR(IT)=AUTCORF(IT,nmeas,tsa,LMEAN)
            CALL AUTCORJ(IT,nmeas,NBINS,tsa,WORK,ACORJ(1,IT),LMEAN)
            CALL STEBJ0(NBINS,ACORJ(1,IT),ACM,ACV,ACE(IT))
          END DO
          CALL AC_INT(NT,ACOR,ACINT)
          CALL AC_INTJ(NT,NBINS,ACORJ,ACINTJ)
          DO IT=0,NT
            CALL STEBJ0(NBINS,ACINTJ(1,IT),ACM,ACV,ACE(IT))
            ABIAS=100*ABS(ACINT(IT)-ACM)/ACINT(IT) ! Bias in %.
            ATAU(IRPT,IT)=ACINT(IT) ! The less biased estimator.
          END DO
        END IF
        call heapsort(nmeas,tsa)
        do imeas=1,nmeas
          tsma(imeas)=tsma(imeas)+tsa(imeas)/nrpt
        end do
C
      end do
      close(iud1)
      call qdf_gnu(iud2,nmeas,tsma)
C
      call steb0(nrpt,act,actm,actv,acte)
      write(iuo,'(" actm =",F15.10," +/-",F15.10)') actm,acte
      write(iuo,'(" em =  ",F15.10," +/-",F15.10)') (-2*actm),(2*acte)
      IF(.NOT.LAUTO) STOP "ana_ts_one: no autocorrelations computed."
C
      WRITE(IUO,*) "Autocorrelation times are on the a*.d file."
      open(iud1,file="a"//cn//"_"//cd//"d"//cl//".d",
     &          form="formatted",status="unknown")
      DO IT=0,NT,NSTEP
        CALL STEB0(NRPT,ATAU(1,IT),ACINT(IT),ACV,ACE(IT))
        ACIV=ns*ACINT(IT)
        ACEV=ns*ACE(IT)
        WRITE(IUD1,'(I6,4F16.4)') IT,ACINT(IT),ACE(IT),ACIV,ACEV
      END DO
      close(iud1)
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
      INCLUDE '../../ForLib/heapsort.f'
      INCLUDE '../../ForLib/nsfun.f'
      INCLUDE '../../ForLib/qdf_gnu.f'
      INCLUDE '../../ForLib/steb0.f'
      INCLUDE '../../ForLib/stebj0.f'
      INCLUDE '../../ForLib/stmean.f'
