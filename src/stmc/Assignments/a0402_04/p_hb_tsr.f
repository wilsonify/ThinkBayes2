      program p_hb_tsr
C Copyright, Bernd Berg, Apr 18 2002. Potts model heat bath updating.
C MC prodcution run to create energy (action variable) time series.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts_hb.com'
      dimension tsa(nmeas)
      include 'lat.dat'
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ltest p_hb_ts."
C
      write(iuo,'(/," p_hb_ts.f: nq =",I3)') nq
      lpr=.true.
      call potts_inithb(lpr) ! Initialize Potts Metropolis MC.
C
      do iequi=1,nequi     ! Sweeps for reaching equilibrium.
        call potts_mchb_r
      end do
      if(ltest) print*,"After equilibration: iact =",iact
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cq//"_"//cd//"d"//cl//"hbr.d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repetitions of nmeas measurement sweeps.
        acpt=zero
        do imeas=1,nmeas
          call potts_mchb_r
          tsa(imeas)=(ONE*iact)/nlink ! Action per link.
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

      include 'potts_inithb.f'
      include 'potts_mchb_r.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts_wghb.f'

      include '../../ForLib/razero.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/write_progress.f'
