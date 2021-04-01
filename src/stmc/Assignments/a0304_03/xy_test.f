      program XY_test
C Copyright, Bernd Berg, Mar 1 2002.  Test for rounding issues.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      include 'lat.dat'
      if(n.ne.2) stop "XY_ts0: set n=2 in on.par (for ana_tson.f)!"
C
      write(iuo,'(/," XY_ts0.f:")')
      call XY_init(.true.) ! Initialize XY Metropolis MC.
      write(iuo,'(" iequi,act:",I6,G24.16)') 0,act
C
      do iequi=1,nequi        ! Sweeps for reaching equilibrium.
        call XY_mtest(iequi)
        if(iequi.ge.3700 .and. iequi.le.3707) 
     & 	  write(iuo,'(" iequi,act:",I6,G24.16)') iequi,act
      end do
C
      stop
      end

      include 'xy_init.f'
      include 'xy_mtest.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'
      include '../../ForLib/write_progress.f'
      include '../../ForLib/razero.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'

      include '../../ForLib/xy_ran.f'
      include '../../ForLib/xy_act.f'
