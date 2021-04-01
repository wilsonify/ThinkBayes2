      subroutine XY_init1(lpr)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright, Bernd Berg, Nov 14 2000.
C Initializes arrays needed for generalized Potts Model MC calculation.
      include 'lat.par'
      include 'mc.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/xy1.com'
      dimension ix(nd)
C
C Initialize:
      if(lpr) call rmaset(iuo,iud1,iseed1,iseed2,'nexiste.pa') ! ran-num
      if(.not.lpr) call rmaset(-iuo,iud1,iseed1,iseed2,'nexiste.pa') 
      call ranmar(xr)                              ! first random number
      acpt=zero                                    ! acceptance rate
      a_min=one*mlink
      a_max=zero
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink)                ! lattice
      call XY_ran1(sta,ns)                   ! XY initial states (random)
      call xy_act1(sta,ipf,ns,nd,act)                         ! XY action
C
      if(.not.lpr) return
      write(iuo,'(" Initial randon number: xr =",1F12.6)') xr
      write(iuo,'(" lat.par: nd,nla         =",1I2,3X,8I4)') nd,nla
      write(iuo,'("          ms,mlink,nlink =",3I10)') ms,mlink,nlink
      write(iuo,'(" mc.par:")')
      write(iuo,'(1X,"iuo,iud1,iseed1,iseed2,    beta")')
      write(iuo,'(        2I4,          2I7,     1F11.6)')
     &                      iuo,iud1,iseed1,iseed2,beta
      write(iuo,'(" Initial action:  act =",G15.6)') act
      return
      end
