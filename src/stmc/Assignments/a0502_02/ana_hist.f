      program ana_hist
C Copyright, Bernd Berg, Jun 27 2002.
C Prepares energy histogram plots and calculates average energy (action).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
      dimension ham(0:mlink),hae(0:mlink),act(nrpt)
C
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="old")
      read(iud1) beta_in,nd_in,nq_in,nla,nlink
      write(iuo,'(/," beta,nlink,nd,nq,nla:",1F16.12,I9,2I3,4I5)') 
     & beta,nlink,nd_in,nq_in,nla
      write(iuo,'(" nequi,nrpt,nmeas:",20X,I9,I6,I10)') nequi,nrpt,nmeas
      call read_steb0(nrpt,iud1,1,nlink,ha,ham,hae,act)
      close(iud1)
C
      open(iud1,file="h"//cd//"d"//cq//"q"//cl//".d",
     & form="formatted",status="unknown")
      do ilink=0,nlink
        if(ham(ilink)>(half/nrpt)) then
          actm=ilink/(nlink*one)
          em=nd*two*((one/nq)-actm) ! Internal energy per site.
          write(iud1,'(I10,4G15.6)') ilink,actm,em,ham(ilink),hae(ilink)
        end if
      end do
      close(iud1)
C
      call steb0(nrpt,act,actm,actv,acte)
      write(iuo,'(" actm =",F16.9,"  +/-",F16.9)') actm,acte
      em=nd*two*((one/nq)-actm) ! Internal energy per site.
      ee=nd*two*acte
      write(iuo,'(" em   =",F16.9,"  +/-",F16.9,/)') em,ee
c
      actm=0.64604*one
      acte=0.00006*one
      em=nd*two*((one/nq)-actm) 
      ee=nd*two*acte
      if(nd==2.and.nq==4) write(iuo,'(" beta=0.53361",
     &  a,/," em   =",F16.9,"  +/-",F16.9,/)')
     &  ' energy from Caselle et. al. NPB 562 (1999) 549:',em,ee
c
      if(nd==3.and.nq==2)
     &  write(iuo,'(a,/," em_A =",F16.9,"  +/-",F16.9,/)')
     &  ' Result in Alves et. al. PRB 41 (1990) 383 notation:',
     &  (em/four),(ee/four)
c
      stop
      end

      include '../../ForLib/potts_actm.f'
      include '../../ForLib/razero.f'
      include '../../ForLib/steb0.f'
      include '../../ForLib/read_steb0.f'
