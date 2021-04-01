      program p_met_tsm
C Copyright, Bernd Berg, Apr 11 2002. Potts model Metropolis.
C MC prodcution run to create energy (action variable) time series,
C using mean values averaged over the updates of the sweep.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      dimension tsa(nmeas)
      include 'lat.dat'
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ltest p_met_tsm."
C
      write(iuo,'(/," p_met_ts.f: nq =",I3)') nq
      lpr=.true.
      call potts_init(lpr) ! Initialize Potts Metropolis MC.
C
      do iequi=1,nequi     ! Sweeps for reaching equilibrium.
        call potts_met
      end do
      if(ltest) print*,"After equilibration: iact =",iact
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cq//"_"//cd//"d"//cl//"m.d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repetitions of nmeas measurement sweeps.
        acpt=zero
        do imeas=1,nmeas
          call potts_met
          tsa(imeas)=potts_actm2(iamin,iamax,ha(iamin))/nlink
          call razero(ha,iamin,iamax)
        end do
        acpt=(acpt/nmeas)/ns
        write(iud1) tsa,acpt,irpt
        call write_progress(iud2,"irpt,iact,acpt:",irpt,iact,acpt)
        print'(" irpt,iact,acpt:",2I10,1F12.4)',irpt,iact,acpt
      end do
      close(iud1)
C
      stop
      end

      include 'potts_init.f'
      include 'potts_met.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_actm2.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts_wght.f'

      include '../../ForLib/razero.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/write_progress.f'
