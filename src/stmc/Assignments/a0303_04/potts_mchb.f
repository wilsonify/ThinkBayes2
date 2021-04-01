      subroutine potts_mchb
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright Bernd Berg, Oct 14 2001.
C MC Heat Bath updating with sequential spin choice.
C Measurements: the action ha and the magnetization hm histograms.
C Faster when the magnetization measurements are eliminated!
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts_hb.com'
      dimension prob_hb(-1:nqm1),iact_a(0:nqm1)
      iamin=iact
      iamax=iact
      prob_hb(-1)=zero
      do is=1,ns
        call ranmar(xr)               
        do iq=0,nqm1
          iact0=0 ! Calculates the action contribution for the state iq.
          do id=1,nd
            iact0=iact0+idel(iq,ista(ipf(id,is)))
     &                 +idel(iq,ista(ipb(id,is)))
          end do
          prob_hb(iq)=whb_tab(iact0,iq)+prob_hb(iq-1)
          iact_a(iq)=iact0                      ! Defines the array iact_a.
        end do
        do iq=0,nqm2
          ptest=prob_hb(iq)/prob_hb(nqm1) ! Heat bath probabilities.
        if(xr.lt.ptest) go to 1
        end do
        iq=nqm1
1       iqnew=iq
        iqold=ista(is)
        ista(is)=iqnew
        iact=iact+iact_a(iqnew)-iact_a(iqold)
        iamin=min(iamin,iact)
        iamax=max(iamax,iact)
        ha(iact)=ha(iact)+one
        nstate(iqold)=nstate(iqold)-1 ! For magnetization measurements.
        nstate(iqnew)=nstate(iqnew)+1 ! For magnetization measurements.
        do iq=0,nqm1          ! Do loop for magnetization measurements.
          hm(nstate(iq),iq)=hm(nstate(iq),iq)+one
        end do
      end do
      ia_min=min(ia_min,iamin)
      ia_max=max(ia_max,iamax)
      return
      end
