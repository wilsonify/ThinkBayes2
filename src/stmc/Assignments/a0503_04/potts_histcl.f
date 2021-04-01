      program potts_histcl
C Copyright, Bernd Berg, Oct 19 2001.
C MC prodcution runs for energy (action) histograms.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      parameter (mlinkp1=mlink+1)
      include '../../ForLib/lat.com'
      include '../../ForLib/potts1.com'
      dimension hcl(ms)
      include 'lat.dat'
C
      call potts1_init(.true.) ! Initialize Potts Metropolis MC:
C
      do iequi=1,nequi         ! Sweeps for reaching equilibrium.
        call potts_cl(ncl)     ! ncl number of clusters.
      end do
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nq,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repititions of nmeas measurement sweeps.
        acpt=zero
        call razero(ha,0,nlink)
        call razero(hcl,1,ms)
        do imeas=1,nmeas
          call potts_cl(ncl)
          hcl(ncl)=hcl(ncl)+one
        end do
        write(iud1) ha,hcl,irpt
        call write_progress(iud2,"irpt,iact,acpt:",irpt,iact,acpt)        
      end do
      close(iud1)
C
      stop
      end

      include 'potts1_init.f'
      include 'potts_cl.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts1_wght.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/steb0.f'
      include '../../ForLib/write_progress.f'
