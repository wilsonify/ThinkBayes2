      program ana_clsw
C Copyright, Bernd Berg, Nov 23 2001.
C Average number and size of SW clusters.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
      dimension hcl(ms),cln(nrpt),clsize(nrpt)
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
        cln(irpt)=zero
        clsize(irpt)=zero
        clsum=zero
        read(iud1) ha,hcl,irpt_in
        do icl=1,ns
          cln(irpt)=cln(irpt)+icl*hcl(icl)
          clsize(irpt)=clsize(irpt)+(xns/icl)*hcl(icl)
          clsum=clsum+hcl(icl)
        end do
        cln(irpt)=cln(irpt)/clsum
        clsize(irpt)=clsize(irpt)/clsum
      end do       
      close(iud1)
C
      call steb0(nrpt,cln,clnm,clnv,clne)
      write(iuo,'(" clnm =    ",1F12.4," +/-",1F12.4)') clnm,clne
      call steb0(nrpt,clsize,clsizem,clv,clsizee)
      write(iuo,'(" clsizem = ",1F12.4," +/-",1F12.4)') clsizem,clsizee
C
      stop
      end

      include '../../ForLib/nsfun.f'
      include '../../ForLib/steb0.f'
