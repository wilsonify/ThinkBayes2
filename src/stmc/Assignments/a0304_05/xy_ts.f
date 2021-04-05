      program XY_ts
C Copyright, Bernd Berg, Feb 10 2002.
C MC production run to create energy (action variable) time series.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      include 'lat.dat'
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ltest XY_ts."
      if(n/=2) stop "XY_ts: set n=2 in on.par (for ana_tson.f)!"
C
      write(iuo,'(/," XY_ts.f:")')
      call XY_init(.true.) ! Initialize XY Metropolis MC.
C
      isw0=1
      do iequi=1,nequi        ! Sweeps for reaching equilibrium.
	if(iequi>=isw0) then
	  lacpt=.true.
	  isw0=2*isw0
        end if
        call XY_met(lacpt)
      end do
      write(iuo,'(" Equilibirum sweeps finished, acpt =",G20.10)') acpt
C
      write(cd,'(I1.1)') nd
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="o02_"//cd//"d"//cl//".d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repititions of nmeas measurement sweeps.
        acpt=zero
        do imeas=1,nmeas
          call XY_met(lacpt)
          tsa(imeas)=act/nlink ! Action per link.
        end do
        acpt=acpt/(nmeas*ns)
        write(iud1) tsa,acpt,irpt
        iact=nint(act)
        call write_progress(iud2,"irpt,iact,acpt:",irpt,iact,acpt)
        call XY_act(sta,ipf,ns,nd,act_test)
        print'(" irpt,acpt,act,act_test:",I5,3F10.3)',
     &           irpt,acpt,act,act_test
      end do
      close(iud1)
C
      stop
      end

      include 'xy_init.f'
      include 'xy_met.f'

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
      include '../../ForLib/steb0.f'

      include '../../ForLib/xy_ran.f'
      include '../../ForLib/xy_act.f'
