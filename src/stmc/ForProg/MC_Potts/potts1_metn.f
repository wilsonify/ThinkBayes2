      subroutine potts1_metn(nhit)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright, Bernd Berg, Nov 15 2000.
C nhit Metropolis updating with sequential spin choice.
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts1.com'
      if(nhit.lt.1.or.nhit.gt.nq) stop "potts1_metn: nhit too large."
      q=nq*ONE
      iamin=iact
      iamax=iact
      do is=1,ns
        istaold=ista(is)
        ista_update=istaold
        do ihit=1,nhit
          call ranmar(xr)
          istanew=int(q*xr)
          if(istanew.ne.ista_update) then
            idact=0
            do id=1,nd
              ista2=ista(ipf(id,is))
              idact=idact+idel(ista2,istanew)-idel(ista2,ista_update)
              ista2=ista(ipb(id,is))
              idact=idact+idel(ista2,istanew)-idel(ista2,ista_update)
            end do
            if(idact.ne.0) then
              call ranmar(xr)
              if(xr.lt.wrat(idact)) then
                ista_update=istanew
                iact=iact+idact
              end if
            else
              ista_update=istanew
            end if
          end if
        end do
        if(ista_update.ne.istaold) then
          acpt=acpt+one
          ista(is)=ista_update
          iamin=min(iamin,iact)
          iamax=max(iamax,iact)
        end if
        ha(iact)=ha(iact)+one
      end do
      ia_min=min(ia_min,iamin)
      ia_max=max(ia_max,iamax)
      return
      end
