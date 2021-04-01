      subroutine potts1_met_r
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright, Bernd Berg, Oct 15 2001.
C Metropolis updating with random spin choice.
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts1.com'
      q=nq*ONE
      iamin=iact
      iamax=iact
      xs=ns*ONE
      do iss=1,ns
        call ranmar(xr)
        is=1+int(xs*xr)
        istaold=ista(is)
        call ranmar(xr)
        istanew=int(q*xr)
        if(istanew.ne.istaold) then
          call ranmar(xr)
          idact=0
          do id=1,nd
            ista2=ista(ipf(id,is))
            idact=idact+idel(ista2,istanew)-idel(ista2,istaold)
            ista2=ista(ipb(id,is))
            idact=idact+idel(ista2,istanew)-idel(ista2,istaold)
          end do
          iactnew=iact+idact
          if(xr.lt.wrat(idact)) then
            ista(is)=istanew
            iact=iactnew
            acpt=acpt+ONE
            iamin=min(iamin,iact)
            iamax=max(iamax,iact)
          end if
        end if
        ha(iact)=ha(iact)+1
      end do
      ia_min=min(ia_min,iamin)
      ia_max=max(ia_max,iamax)
      return
      end
