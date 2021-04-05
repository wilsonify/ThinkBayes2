      program ana_mag
C Copyright, Bernd Berg, Oct 15, 2001.  Prepares histogram 
C plots for the magnetizations and calculates their averages.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3,ciq*2
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
      dimension hm(0:ms,0:nqm1),hmm(0:ms,0:nqm1),hme(0:ms,0:nqm1)
      dimension qm(nrpt,0:nqm1),qmm(0:nqm1),qme(0:nqm1)
      ltest=.true.
C
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="old")
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="old")
      read(iud1) beta_in,nd_in,nq_in,nla,nlink
      ns=nsfun(nla,nd)
      CALL RMAG_STEB0(nrpt,iud1,0,ms,nqm1,mlink,ns,hm,hmm,hme,qm,ha)
      close(iud1)
C
      do iq=0,nqm1
        call steb0(nrpt,qm(1,iq),qmm(iq),qv,qme(iq))
        write(iuo,'(I6,"q, qmm =",F16.9,"  +/-",F16.9)') 
     &              (iq+1),qmm(iq),qme(iq)
        write(ciq,'(I2.2)') (iq+1)
        open(iud1,file="m"//cd//"d"//cq//"q"//cl//"_"//ciq//".d",
     &   form="formatted",status="unknown")
        do is=0,ns
        if(hmm(is,iq)>half) then
          xiqm=is/(ns*one)
          write(iud1,'(I10,3G15.6)') is,xiqm,hmm(is,iq),hme(is,iq)
        end if
        end do
        close(iud1)
      end do
C
      stop
      end

      include '../../ForLib/nsfun.f'
      include '../../ForLib/potts_actm.f'
      include '../../ForLib/razero.f'
      include '../../ForLib/steb0.f'
      include '../../ForLib/rmag_steb0.f'
