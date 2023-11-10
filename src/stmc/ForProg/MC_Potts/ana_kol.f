      program ana_kol
C Copyright, Bernd Berg, Jun 27 2003. Upper and lower Kolmogorov 
C            tests to check whether the binned data are Gaussian.
C            Compare assignment a0206_03.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
      dimension ham(0:mlink),hae(0:mlink),act(nrpt),Fxct(nrpt)
C
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="old")
      read(iud1) beta_in,nd_in,nq_in,nla,nlink
      write(iuo,'(/," Kolmogorov test on nrpt binned Gaussian data:")') 
      write(iuo,'(" beta,nlink,nd,nq,nla:",1F16.12,I9,2I3,4I5)') 
     & beta,nlink,nd_in,nq_in,nla
      write(iuo,'(" nequi,nrpt,nmeas:",20X,I9,I6,I10)') nequi,nrpt,nmeas
      call read_steb0(nrpt,iud1,0,nlink,ha,ham,hae,act)
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
C
      open(iud1,file="gau_df.d",form="formatted",status="unknown")
      call heapsort(nrpt,act)
      F=-one/(2*nrpt)
      do irpt=1,nrpt
        F=F+one/nrpt
        sdv=sqrt(actv)
        X=(act(irpt)-actm)/sdv
        Fxct(irpt)=gau_df(X)
        write(iud1,'(I6,3G16.7)') irpt,X,F,Fxct(irpt)
      end do
      close(iud1)
C
      CALL KOLM1(nrpt,Fxct,DEL1,DEL2,Q1,Q2)
      WRITE(IUO,'(" nrpt,DEL1,DEL2:",I6,2F10.2)') nrpt,DEL1,DEL2
      WRITE(IUO,'(" Q1,Q2:         ",6X,2F10.2)') Q1,Q2
C
      stop
      end

      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gau_df.f'
      include '../../ForLib/potts_actm.f'
      include '../../ForLib/razero.f'
      include '../../ForLib/read_steb0.f'
      include '../../ForLib/heapsort.f'
      include '../../ForLib/steb0.f'

      include '../../ForLib/kolm1.f'
