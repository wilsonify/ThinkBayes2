      program ana_clw
C Copyright, Bernd Berg, Nov 23 2001.
C Average size of Wolff clusters.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
      dimension hcl(ms),clwsize(nrpt)
C
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="old")
      read(iud1) beta_in,nd_in,nq_in,nla,nlink
      write(iuo,'(" beta,nlink,nd,nq,nla:",1F16.12,I9,2I3,4I5)') 
     & beta,nlink,nd_in,nq_in,nla
C
      ns=nsfun(nla,nd)
      xns=ns*one
      do irpt=1,nrpt
        clwsize(irpt)=zero
        clsum=zero
        read(iud1) ha,hcl,irpt_in
        do icl=1,ns
          clwsize(irpt)=clwsize(irpt)+icl*hcl(icl)
          clsum=clsum+hcl(icl)
        end do
        clwsize(irpt)=clwsize(irpt)/clsum
      end do       
      close(iud1)
C
      call steb0(nrpt,clwsize,clwsizem,clv,clwsizee)
      write(iuo,'(" clwsizem =",1F12.4," +/-",1F12.4)')clwsizem,clwsizee
C
      stop
      end

      include '../../ForLib/nsfun.f'
      include '../../ForLib/steb0.f'
