      subroutine XY_met(lacpt)
C Copyright Bernd Berg, Feb 10 2002. XY model: Sequential Metropolis
C                 updating. Acceptance rate self-adjusting to 1/2.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(n=2)
      include 'lat.par'
      include 'mc.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      data dphi/tpi/, facpt/zero/, sweep/zero/
      save dphi,facpt
      amin=act
      amax=act
      acpt0=zero
      do is=1,ns
        phi=dphi*(rmafun()-half)
        c_phi=cos(phi)
        s_phi=sin(phi)
        x=sta(1,is)
        y=sta(2,is)
        xnew=+c_phi*x-s_phi*y
        ynew=+s_phi*x+c_phi*y
        dx=xnew-x
        dy=ynew-y
        dact=zero
        do id=1,nd
          dact=dact+sta(1,ipf(id,is))*dx+sta(2,ipf(id,is))*dy
          dact=dact+sta(1,ipb(id,is))*dx+sta(2,ipb(id,is))*dy
        end do
        if(dact.lt.zero) then
        if(rmafun().ge.exp(beta*dact)) go to 1
        end if
          sta(1,is)=xnew
          sta(2,is)=ynew
          act=act+dact
          acpt0=acpt0+one
          amin=min(amin,act)
          amax=max(amax,act)
 1      continue
      end do
      acpt=acpt+acpt0
      facpt=facpt+two*acpt0/ns
      sweep=sweep+one
      if(lacpt) then
	facpt=max(facpt,one/ns)/sweep
	dphi=min(tpi,facpt*dphi)
	facpt=zero
	sweep=zero
	lacpt=.false.
      end if 
      a_min=min(a_min,amin)
      a_max=max(a_max,amax)
      return
      end
