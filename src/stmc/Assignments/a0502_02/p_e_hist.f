      program p_e_hist 
C Copyright, Bernd Berg, Nov 6 2002.
C Event driven MC simulation: production runs for the
C       histograms ha of the energy (action variable).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'p_eds.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/p_eds.com'
      include 'lat.dat'
C
      call p_e_init(.true.) ! Initialization.
C
      do iequi=1,nequi      ! Sweeps for reaching equilibrium.
        call p_e_mc(iuo)
        if(iequi==(nequi/4).or.iequi==(nequi/2)) call Pcase_sort
      end do
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') ml ! Assumes ml=nl=nla(i), i=1,...,nd.
      open(iud1,file="p"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nq,nla,nlink,nequi,nrpt
      do irpt=1,nrpt ! nrpt repititions of nmeas measurement sweeps.
        call razero(ha,0,nlink)
        do imeas=1,nmeas
          call p_e_mc(iuo)
        end do
        write(iud1) ha,irpt
        call write_progress(iud2,"irpt,iact,beta:",irpt,iact,beta)
      end do
      close(iud1)
C
      stop
      end

      include 'p_e_init.f'
      include 'p_e_mc.f'

      include '../../ForLib/heap_per.f'
      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/p_e_compare.f'
c     include '../../ForLib/p_etabs_update.f'
      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/write_progress.f'
