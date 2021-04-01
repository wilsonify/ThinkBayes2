      program O2_ts0
C Copyright, Bernd Berg, Feb 10 2002.
C MC prodcution run to create energy (action variable) time series.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      include 'lat.dat'
      if(n.ne.2) stop "o2_ts: set n=2 in on.par (for ana_tson.f)!"
C
      write(iuo,'(/," o2_ts.f:")')
      call on_init(.true.) ! Initialize O2 Metropolis MC.
C
      do iequi=1,nequi        ! Sweeps for reaching equilibrium.
        call o2_met
      end do
C
      write(cd,'(I1.1)') nd
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="o02_"//cd//"d"//cl//".d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repititions of nmeas measurement sweeps.
        acpt=zero
        do imeas=1,nmeas
          call o2_met
          tsa(imeas)=act/nlink ! Action per link.
        end do
        acpt=acpt/(nmeas*ns) ! Acceptance rate.
        write(iud1) tsa,acpt,irpt
        iact=nint(act)
        call write_progress(iud2,"irpt,iact,acpt:",irpt,iact,acpt)
        call on_act(sta,ipf,n,ns,nd,act_test)
        print'(" irpt,acpt,act,act_test:",I5,3F10.3)',
     &           irpt,acpt,act,act_test
      end do
      close(iud1)
C
      stop
      end

      include 'on_init.f'
      include 'o2_met.f'

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

      include '../../ForLib/on_ran.f'
      include '../../ForLib/on_act.f'
