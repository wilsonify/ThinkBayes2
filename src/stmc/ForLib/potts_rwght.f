      SUBROUTINE POTTS_RWGHT(nlink,ha,hae,beta0,beta,hb,hbe)
C Copyright, Bernd Berg, Dec 9, 2000. 
C Re-weighting of the histogram ha() and its error bars hae() at beta0 
C              to the histogram hb() and its error bars hbe() at beta.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension ha(0:nlink),hae(0:nlink),hb(0:nlink),hbe(0:nlink)
c
      hasum=zero
      act0m=zero
      do iact=0,nlink
        hasum=hasum+ha(iact)
        act0m=act0m+iact*ha(iact)
      end do
      act0m=act0m/hasum
c     
      hbsum=zero
      do iact=0,nlink
        hb(iact)=zero
        hbe(iact)=zero
        if(ha(iact)>half) then
          a=(beta-beta0)*2*(iact-act0m)
          hb(iact)=exp(log(ha(iact))+a)
          hbe(iact)=exp(log(hae(iact))+a)
          hbsum=hbsum+hb(iact)
        end if ! Else hb(iact) is, as ha(iact), zero.
      end do
      factor=hasum/hbsum
      do iact=0,nlink
        hb(iact)=factor*hb(iact)
        hbe(iact)=factor*hbe(iact)
      end do
c     
      RETURN
      END
