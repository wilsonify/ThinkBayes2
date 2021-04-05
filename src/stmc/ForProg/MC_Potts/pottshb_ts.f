      program pottshb_ts
C Copyright, Bernd Berg, June 20 2002.
C Potts time series (ts) with Heat Bath updating.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts_hb.com'
      include 'lat.dat'
      dimension es_ts(0:nequi,2),act(0:nequi,2)
C
C Initialize Potts Metropolis MC:
      lpr=.true.
      call potts_inithb(lpr)
C
      write(iuo,*) '   '
      write(iuo,'( " nq,beta =",I4,1F10.2)') nq,beta
      call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)
c
      write(iuo,*) '   '
      write(iuo,*) 'Results: iequi,es,act on pottshb_ts.d.'
      open(iud1,file='pottshb_ts.d',form='formatted',status='unknown')
      do irpt=1,2
        if(irpt==2) call potts_order(ista,iact,nd,ns,1)
        if(irpt==2) call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)
        do iequi=0,nequi
          if(iequi>=1) call potts_mchb
          act(iequi,irpt)=(iact*one)/(nlink*one)
          es_ts(iequi,irpt)=two*nd/(one*nq)-two*nd*act(iequi,irpt)
          if(irpt==2) write(iud1,'(1I10,4F15.6)')
     &      iequi,(es_ts(iequi,i),i=1,2),(act(iequi,j),j=1,2)
        end do
      end do
      close(iud1)
C
      stop
      end

      include 'potts_inithb.f'
      include 'potts_mchb.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_order.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts_wghb.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'

