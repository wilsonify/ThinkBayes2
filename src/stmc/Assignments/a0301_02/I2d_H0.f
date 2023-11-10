      program I2d_H0
C 2d Ising model: Energy histogram from naive sampling.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud=10,isd1=1,isd2=2,ndat=100 000)
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include 'lat.dat'
C
      call rmaset(iuo,iud,isd1,isd2,'ranmar.d')
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink)
      call potts_act_tab(idel,nqm1)
      call razero(ha,0,nlink)
      do idat=1,ndat
        call potts_ran(ista,ns,nq)
        call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)
        ha(iact)=ha(iact)+one
        iE=2*iact-nlink
        if(ndat*((10*idat)/ndat)==(10*idat))
     &  write(iuo,'(" idat,iact,iE:",3I10)') idat,iact,iE
      end do
C
      open(iud,file='I2d_H0.d',form='formatted',status='unknown')
      do iact=0,nlink
        iE=nlink-2*iact
        es=(iE*ONE)/(ns*ONE)
        p=ha(iact)/ndat
        pe=sqrt( p*(one-p)/(ndat-1) ) ! Binomial error bar.
        he=ndat*pe
        h=ha(iact)
        if(h>0.1d0) write(iud,'(1I6,5F12.5)') iE,es,p,pe,h,he
      end do
      close(iud)
C
      STOP
      END


      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/ipointer.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'
      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/razero.f'
