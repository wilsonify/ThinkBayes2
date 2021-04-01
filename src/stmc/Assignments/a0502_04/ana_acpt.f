      program ana_acpt
C Copyright, Bernd Berg, Oct 12 2002.
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
      dimension acpt(nrpt)
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
      do irpt=1,nrpt
        read(iud1) ha,acpt(irpt),jrpt
      end do
      close(iud1)
      call steb0(nrpt,acpt,acptm,av,acpte)
      write(iuo,'(" acpt =",F10.6," +/-",F10.6)') acptm,acpte
      write(iuo,'(" CPU time ratio   =",F10.6)') (64.d0/8.4d0)
      write(iuo,'(" Efficiency ratio =",F10.6," for random update.")') 
     &              8.4d0/(64.d0*acptm)
C
      stop
      end

      include '../../ForLib/steb0.f'
