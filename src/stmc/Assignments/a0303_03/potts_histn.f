      program potts_histn
C Energy precision MC calculations.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      parameter (mlinkp1=mlink+1)
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
C
      call potts_init(.true.) ! Initialize Potts Metropolis MC:
C
      do iequi=1,nequi        ! Sweeps for reaching equilibrium.
        call potts_metn(nhit)
      end do
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".n",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nq,nla,nlink,nequi,nrpt,nmean
      do irpt=1,nrpt ! nrpt repititions of nmeas measurement sweeps.
        acpt=zero
        call razero(ha,0,nlink)
        do imeas=1,nmeas
          call potts_metn(nhit)
        end do
        write(iud1) ha
        open(iud2,file="progress.d",form="formatted",status="unknown")
          write(iud2,'(" irpt,iact,acpt:",2I12,1F14.8)')
     &    irpt,iact,acpt/(nmeas*ns)
        close(iud2)
      end do
      close(iud1)
C
      stop
      end

      include 'potts_init.f'
      include 'potts_metn.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_order.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts_wght.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/steb0.f'
