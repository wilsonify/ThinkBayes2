      program I2d_rw0
C 2d Ising model: Energy histogram from re-weighting of naive sampling.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud=10,beta=0.2d0)
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      dimension hae(0:mlink),hb(0:mlink),hbe(0:mlink)
      include 'lat.dat'
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink)
      print*,"nla,nlink:",nla,nlink
C
      open(iud,file='I2d_H0.d',form='formatted',status='old')
      idat=0
      call razero(ha,0,mlink)
      call razero(hae,0,mlink)
1     continue
      read(iud,'(1I6,5F12.5)',end=2) iE,es,p,pe,h,he
        idat=idat+1
        iact=-(iE-nlink)/2
        if(idat==1) iamin=iact
c       print'(" idat,iE,iact:",3I9)',idat,iE,iact
c       print*,'idat,iE,h,he:',idat,iE,h,he
        ha(iact)=h
        hae(iact)=he
      go to 1
2     continue
      iamax=iact
      ndat=idat
      close(iud)
C
      call potts_rwght(nlink,ha,hae,zero,beta,hb,hbe)
C
      open(iud,file='I2d_Hb.d',form='formatted',status='unknown')
      do iact=iamin,iamax,2
        iE=-2*iact+nlink
        es=iE/(ns*ONE)
        write(iud,'(1I6,5F12.5)') iE,es,hb(iact),hbe(iact)
      end do
      close(iud)
c
      STOP
      END

      include '../../ForLib/potts_rwght.f'
      include '../../ForLib/razero.f'
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
