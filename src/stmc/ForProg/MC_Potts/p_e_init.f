      subroutine p_e_init(lpr)
C Copyright, Bernd Berg, Nov 6 2002.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Initializes arrays needed for generalized Potts Model MC calculation.
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'p_eds.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/p_eds.com'
      dimension ix(nd)
C Initializations:
      if(lpr) call rmaset(iuo,iud1,iseed1,iseed2,'nexiste.pa') 
      if(.not.lpr) call rmaset(-iuo,iud1,iseed1,iseed2,'nexiste.pa') 
      call ranmar(xr) ! Random numbers initialized; first random number.
      ia_min=mlink
      ia_max=0
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink)   ! Lattice initialized.
      call potts_act_tab(idel,nqm1)     ! Potts action table calculated.
      call potts_ran(ista,ns,nq)            ! Disordered (random) start.
c      call potts_order(ista,iact,nd,ns,nqm1)       ! Ordered start.
      call potts_act(ista,ipf,idel,ns,nqm1,nd,iact) ! Action calculated.
      call razero(ha,0,nlink)             ! initialize action histogram.
      call p_e_tabs(beta)                      ! Event driven MC tables.
C
      if(.not.lpr) return
      write(iuo,'(" Initial randon number: xr =",1F12.6)') xr
      write(iuo,'(" lat.par: nd,nq,nla =",1I2,1I3,3X,8I4)')
     &                       nd,nq,nla
      write(iuo,'("          ms,mlink  =  ",2I10)') ms,mlink
      write(iuo,'(" mc.par:")')
      write(iuo,'(" iuo,iud1,iseed1,iseed2,    beta")')
      write(iuo,'(        2I4,           2I7,    1F11.6)')
     &              iuo,iud1,iseed1,iseed2,beta
      write(iuo,'(" Initial action:     iact  =",1I10)') iact
      write(iuo,'(" nequi,nrpt,nmeas:      ",3I15)') nequi,nrpt,nmeas
      write(iuo,'("Pstay,Pmove,Pstay+Pmove:",3G15.7)')
     &             Pstay,Pmove,(Pstay+Pmove)
C
      return
      end 


      subroutine p_e_tabs(beta)
C Copyright, Bernd Berg, Jan 13 2002.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'potts.par'
      include 'p_eds.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/p_eds.com'
      dimension iqarray(n2d),nqa(n2d)
      dimension nact_old(0:n2dp1,mcase),nact_new(0:n2dp1)
C
      xnsi=one/ns
      ncase=0
      do id=1,n2d
        nqa(id)=nq ! Sets the box dimension for the ixcor call.
      end do
C
      do icon0=1,nconf0 ! Loop over the neighbor configurations.
        call ixcor(iqarray,nqa,icon0,n2d)
        do iq0=0,nqm1   ! Loop over the central state.
          do id=0,n2d
            nact_new(id)=0
          end do
          iconf=icon0+iq0*nconf0
          do iq=0,nqm1
            ia=0
            do id=1,n2d
              if(iq.eq.iqarray(id)) ia=ia+1
            end do
            if(iq.ne.iq0) nact_new(ia)=nact_new(ia)+1
            if(iq.eq.iq0) nact_new(n2dp1)=ia
          end do
          call p_e_compare(nact_old,nact_new,n2d,mcase,ncase,icase)
          if(ncase.gt.mcase) stop "p_e_tabs: enlarge mcase!"
	  if(icase.gt.ncase) then
	    icase=ncase
            Peds0=exp(2*nact_new(n2dp1)*beta)
            Psum=Peds0
            do id=0,n2d
              Psum=Psum+nact_new(id)*exp(2*id*beta)
            end do
            qmove(ncase)=xnsi*(ONE-Peds0/Psum)
            do id=0,n2d
              Peds1(id,ncase)=xnsi*exp(2*id*beta)/Psum
            end do
          end if
          IACASE(iconf)=icase
          ia_array(icase)=nact_new(n2dp1)
        end do
      end do
C
      if(ncase.ne.mcase) then
        write(iuo,*) "p_e_init->p_e_tabs:"
        write(iuo,'(" Re-run with mcase = ncase =",I6,".")') ncase
        stop "p_e_tabs: mcase.ne.ncase - change in p_e.par."
      end if
C
      do icase=1,mcase
        Pcase(icase)=zero
        NAcase(icase)=0
        do is=1,ns
          Index(is,icase)=0
        end do
      end do
C
      Pstay=zero
      do is=1,ns
        iq0=ista(is)
        iconf=1+iq0*nconf0
        ifct=1
        ia0=0
        do id=1,nd
          iqf=ista(ipf(id,is))
          iconf=iconf+ifct*iqf
          ifct=ifct*nq
          iqb=ista(ipb(id,is))
          iconf=iconf+ifct*iqb
          ifct=ifct*nq
          ia0=ia0+idel(iq0,iqf)
          ia0=ia0+idel(iq0,iqb)
        end do
        icase=IACASE(iconf)
        IScase(is)=icase
        NAcase(icase)=NAcase(icase)+1
        Index(NAcase(icase),icase)=is
        Index(is,mcase+1)=NAcase(icase)
        Pcase(icase)=Pcase(icase)+qmove(icase)
      end do
c
      Pmove=zero
      do icase=1,mcase
        Pmove=Pmove+Pcase(icase)
      end do
      Pstay=One-Pmove
c
      return
      end
