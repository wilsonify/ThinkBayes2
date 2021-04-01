      program domain
C Copyright, Bernd Berg, Mar 15 2002.
C Prepares picture of a frozen O(2) domain wall configuration.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cl*3,ctext*53
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      dimension ix(nd)
      include 'lat.dat'
      if(n.ne.2) stop "XY_ts0: set n=2 in on.par (for ana_tson.f)!"
      ltest=.true.
C
      write(iuo,'(/," domain.f:")')
      call XY_init(.true.) ! Initialize XY Metropolis MC.
C
      do iequi=1,nequi        ! Sweeps for reaching equilibrium.
        call XY_met0
      end do
      es=-(2*act)/nlink
      write(iuo,'(" act,es:",2F20.10)') act,es
c
      open(iud1,file='domain.plt',form='formatted',status='unknown')
      ctext='set xlabel "x"'
      write(iud1,'(A14)') ctext
      ctext='set ylabel "y"'
      write(iud1,'(A14)') ctext
      write(iud1,'("set xrange [-1:10]")')
      write(iud1,'("set yrange [-1:10]")')
      do is=1,ns
        call ixcor(ix,nla,is,nd)
        x=ix(1)+0.8d0*sta(1,is)
        y=ix(2)+0.8d0*sta(2,is)
        write(iud1,'("set arrow from",I3,",",I1," to",F8.3,",",F6.3)')
     &        ix,x,y
      end do 
      ctext='plot "out_of_range.dat" using 1:2 notitle with line 1'
      write(iud1,'(A53)') ctext
      write(iud1,'("pause -1")')
      close(iud1)
C
      stop
      end

      include 'xy_init.f'
      include 'xy_met0.f'

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
