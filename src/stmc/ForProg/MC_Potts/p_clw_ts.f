      program p_clw_ts
C Copyright, Bernd Berg, Oct 28 2002. Potts model Wolff cluster.
C MC prodcution run to create energy (action variable) time series.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cq*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts1.com'
      dimension tsa(nmeas),hflip(ms)
      include 'lat.dat'
      ltest=.false.
      if(ltest) stop "ltest p_clw_ts."
      ltest=.true.
C
      write(iuo,'(/," p_clw_ts.f: nq =",I3)') nq
      lprt=.true.
      call potts1_init(.true.) ! Initialize Potts MC.
C
      do iequi=1,nequi     ! Sweeps for reaching equilibrium.
        call potts_clw(nclsize)
      end do
      if(ltest) print*,"After equilibration: iact =",iact
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="p"//cq//"_"//cd//"d"//cl//"clw.d",
     &     form="unformatted",status="unknown")
      write(iud1) beta,nd,nla,nlink,nequi,nrpt,nmeas
      do irpt=1,nrpt ! nrpt repetitions of nmeas measurement sweeps.
        nflip=0
        imeas=0
1       continue ! imeas=1,nmeas loop.
          call potts_clw(nclsize)
          nflip=nflip+nclsize
          hflip(nclsize)=hflip(nclsize)+one
          if(nflip>=ns) then
            imeas=imeas+1
            nflip=nflip-ns
            tsa(imeas)=(ONE*iact)/nlink ! Action per link.
          end if
        if(imeas<nmeas) go to 1
c       print'(" irpt,iact,nclsize:",3I10)',irpt,iact,nclsize
        print'(" irpt,imeas,iact,nflip:",4I10)',
     &           irpt,imeas,iact,nflip
        write(iud1) tsa,hflip,irpt
        xflip=one*nflip
        call write_progress(iud2,"irpt,iact,nflip:",irpt,iact,xflip)
      end do
      close(iud1)
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
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts1_wght.f'

      include '../../ForLib/razero.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/write_progress.f'
