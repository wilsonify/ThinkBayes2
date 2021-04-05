      subroutine o2_met
C Copyright Bernd Berg, Feb 11 2002.
C XY model: Sequential Metropolis updating.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(n=2)
      include 'lat.par'
      include 'mc.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      amin=act
      amax=act
      do is=1,ns
2       continue
          xnew=two*(rmafun()-half)
          ynew=two*(rmafun()-half)
	  sum=xnew**2+ynew**2
	if(sum>one) go to 2 ! xnew**2+ynew**2 inside the unit circle.
	fn=one/sqrt(sum)
	xnew=fn*xnew
	ynew=fn*ynew
        dx=xnew-sta(1,is)
        dy=ynew-sta(2,is)
        dact=zero
        do id=1,nd
          dact=dact+sta(1,ipf(id,is))*dx+sta(2,ipf(id,is))*dy
          dact=dact+sta(1,ipb(id,is))*dx+sta(2,ipb(id,is))*dy
        end do
        if(dact<zero) then
        if(rmafun()>=exp(beta*dact)) go to 1
        end if
          sta(1,is)=xnew
          sta(2,is)=ynew
          act=act+dact
          acpt=acpt+one
          amin=min(amin,act)
          amax=max(amax,act)
 1      continue
      end do
      a_min=min(a_min,amin)
      a_max=max(a_max,amax)
      return
      end
