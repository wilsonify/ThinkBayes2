      program potts_ts
C Copyright, Bernd Berg, Dec 11 2000.
C Potts time series (ts) with systematic Metropolis updating.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
      dimension es_ts(0:nequi,2),act(0:nequi,2)
C
C Initialize Potts Metropolis MC:
      lpr=.true.
      call potts_init(lpr)
C
      write(iuo,*) '   '
      write(iuo,*) 'Results: iequi,es,act on potts_ts.d.'
      open(iud1,file='potts_ts.d',form='formatted',status='unknown')
      do irpt=1,2
        if(irpt.eq.2) call potts_order(ista,iact,nd,ns,1)
c       if(irpt.eq.2) call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)
        do iequi=0,nequi
          if(iequi.ge.1) call potts_met
          act(iequi,irpt)=(iact*one)/(nlink*one)
          es_ts(iequi,irpt)=two*nd/(one*nq)-two*nd*act(iequi,irpt)
          if(irpt.eq.2) write(iud1,'(1I10,4F15.6)') 
     &      iequi,(es_ts(iequi,i),i=1,2),(act(iequi,i),i=1,2)
        end do
      end do
      close(iud1)
C
      stop
      end

      include 'potts_init.f'
      include 'potts_met.f'


      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_order.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts_wght.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'

