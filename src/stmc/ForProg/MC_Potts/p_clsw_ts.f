      program p_clsw_ts
C Copyright, Bernd Berg, Nov 26 2002. Potts model Swendsen-Wang cluster.
C MC production run to create energy (action variable) time series.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts1.com'
      dimension tsa(nmeas)
      include 'lat.dat'
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ltest p_cl_ts."
C
      write(iuo,'(/," p_cl_ts.f: nq =",I3)') nq
      lprt=.true.
      call potts1_init(.true.) ! Initialize Potts MC.
C
      do iequi=1,nequi     ! Sweeps for reaching equilibrium.
        call potts_cl(ncl)
      end do
      if(ltest) print*,"After equilibration: iact =",iact
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cq//"_"//cd//"d"//cl//"cl.d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repetitions of nmeas measurement sweeps.
        do imeas=1,nmeas
          call potts_cl(ncl)
          tsa(imeas)=(ONE*iact)/nlink ! Action per link.
        end do
        write(iud1) tsa,irpt
        call write_progress(iud2,"irpt,iact,one:",irpt,iact,one)
        print'(" irpt,iact:",2I10,1F12.4)',irpt,iact
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
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/write_progress.f'
