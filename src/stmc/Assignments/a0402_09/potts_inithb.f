      subroutine potts_inithb(lpr)
C Copyright, Bernd Berg, Oct 14 2001.
C Initializes arrays needed for generalized Potts Model MC heat bath.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts_hb.com'
      dimension ix(nd)
C
C Initialize:
      if(lpr) call rmaset(iuo,iud1,iseed1,iseed2,'nexiste.pa') ! ran-num
      if(.not.lpr) call rmaset(-iuo,iud1,iseed1,iseed2,'nexiste.pa') 
      call ranmar(xr) ! first random number
      acpt=zero                                        ! acceptance rate
      ia_min=mlink
      ia_max=0
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink)           ! lattice
      call potts_act_tab(idel,nqm1)                 ! potts action table
      call potts_ran(ista,ns,nq)         ! potts initial states (random)
      call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)       ! potts action
      call potts_wghb(whb_tab,beta,H0,nd,nqm1)  ! weights
      call razero(ha,0,nlink)              ! initialize action histogram
      do iq=0,nqm1
        nstate(iq)=0
        do is=0,ms ! Initialize magnetization arrays.
          hm(is,iq)=zero
        end do
      end do 
      do is=0,ms ! Initialize magnetization arrays.
        nstate(ista(is))=nstate(ista(is))+1
      end do 
C
      if(.not.lpr) return
      write(iuo,'(" Initial randon number: xr =",1F12.6)') xr
      write(iuo,'(" lat.par: nd,nq,nla =",1I2,1I3,3X,8I4)')
     &                       nd,nq,nla
      write(iuo,'("          ms,mlink  =  ",2I10)') ms,mlink
      write(iuo,'(" mc.par:")')
      write(iuo,'(1X,"iuo,iud1,iseed1,iseed2,    beta")')
      write(iuo,'(        2I4,          2I7,     1F11.6)')
     &                      iuo,iud1,iseed1,iseed2,beta
      write(iuo,'(" Initial action:     iact  =",1I10)') iact
      write(iuo,*) "Initial states:",nstate
      return
      end
