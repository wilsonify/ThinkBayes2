      program o3_tshb 
C Copyright, Bernd Berg, May 8 2002.
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
      ltest=.true.
      ltest=.false.
      if(ltest) stop "O3_ts: ltest."
      if(n.ne.3) stop "O3_tshb: set n=3 in on.par (for ana_tson.f)!"
C
      call O3_init(.true.) ! Initialize XY Metropolis MC.
C
      do iequi=1,nequi     ! Sweeps for reaching equilibrium.
        call O3_mchb
      end do
      call O3_act(sta,ipf,ns,nd,act_renew)
      print'(" act,act_test:",2F20.12," after nequi.")',act,act_renew
      act=act_renew
C
      write(cd,'(I1.1)') nd
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="o03hb_"//cd//"d"//cl//".d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repititions of nmeas measurement sweeps.
        do imeas=1,nmeas
          call O3_mchb
          tsa(imeas)=act/nlink ! Action per link.
        end do
        write(iud1) tsa,one,irpt
        iact=nint(act)
        call write_progress(iud2,"irpt,iact,acpt:",irpt,iact,one)
        call O3_act(sta,ipf,ns,nd,act_renew)
        print'(" irpt,act,act_renew:",I6,2F12.3)',irpt,act,act_renew
        act=act_renew
      end do
      close(iud1)
C
      stop
      end

      include 'o3_init.f'
      include 'o3_mchb.f'

      include '../../ForLib/o3_ran.f'
      include '../../ForLib/o3_act.f'

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
