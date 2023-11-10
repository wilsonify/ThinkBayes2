      subroutine potts1_met
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright, Bernd Berg, Nov 15 2000.
C Metropolis updating with sequential spin choice.
C Small wrat() array of potts1.com used.
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts1.com'
      q=nq*ONE
      iamin=iact
      iamax=iact
      do is=1,ns
        istaold=ista(is)
        call ranmar(xr)
        istanew=int(q*xr)
        if(istanew/=istaold) then
          idact=0
          do id=1,nd
            ista2=ista(ipf(id,is))
            idact=idact+idel(ista2,istanew)-idel(ista2,istaold)
            ista2=ista(ipb(id,is))
            idact=idact+idel(ista2,istanew)-idel(ista2,istaold)
          end do
          if(idact/=0) then
          call ranmar(xr)
            if(xr<wrat(idact)) then
              ista(is)=istanew
              iact=iact+idact
              acpt=acpt+one
              iamin=min(iamin,iact)
              iamax=max(iamax,iact)
            end if
          else
            ista(is)=istanew
            acpt=acpt+one
          end if
        end if
      ha(iact)=ha(iact)+one
      end do
      ia_min=min(ia_min,iamin)
      ia_max=max(ia_max,iamax)
      return
      end
