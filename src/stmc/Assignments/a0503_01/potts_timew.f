      program potts_timew
C Bernd Berg, Oct 19 2001.
C MC prodcution runs for energy (action) histograms.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      parameter (mlinkp1=mlink+1)
      include '../../ForLib/lat.com'
      include '../../ForLib/potts1.com'
      include 'lat.dat'
C
      call potts1_init(.true.) ! Initialize Potts Metropolis MC:
C
      nflip=0
      ncall=0
1     ncall=ncall+1  ! Calls for reaching equilibrium.
        call potts_clw(nclsize)
        nflip=nflip+nclsize
      if(nflip.le.ns*nequi) go to 1
      ratio=(one*ncall)/(one*nequi)
      write(iuo,'(" nequi,ncall,ratio:",2I10,1F12.6)')
     &              nequi,ncall,ratio
C
      stop
      end

      include 'potts1_init.f'
      include 'potts_clw.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_order.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts1_wght.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/steb0.f'
