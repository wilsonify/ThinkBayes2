      program potts_t
C Timing of Potts Model 1-hit Metropolis updating.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
C
C Initialize Potts Metropolis MC:
      lpr=.false.
      call potts_init(lpr)
C
      do iequi=1,nequi
        call potts_metn(nhit)
      end do
      print*,"With potts_metn: nhit,nequi =",nhit,nequi,
     &       " done. iact =",iact
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

