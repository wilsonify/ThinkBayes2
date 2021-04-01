      SUBROUTINE POTTS_ZLN(nlink,namin,beta0,b,ha,hasum,iopt,
     &                     Zln,Aln,A2ln)
C Copyright Bernd Berg, May 12 2002.
C Potts model ln of the partition function Z and related variables.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension b(0:nlink),ha(0:nlink),hasum(0:nlink)
C
      iloop_ha=0 ! Counts runs through the ha(iact).gt.half loop.
      if(namin.le.0) stop "POTTS_ZLN: namin false."
      do iact=0,nlink
        ha(iact)=hasum(iact)-ha(iact) ! Jackknife histogram.
        if(ha(iact).gt.half) then
          iloop_ha=iloop_ha+1
          if(iloop_ha.eq.1) then
            a=-(beta0-b(iact))*namin ! Ferdinand-Fisher normalization.
            Zln=log(ha(iact))+two*((beta0-b(iact))*iact+a)
            Zln1_max=Zln
            Aln=Zln+log(iact*one)
            A2ln=Zln+two*log(iact*one)
          else
            a=a+(b(iact)-b(iact_old))*iact_old
            Zln1=log(ha(iact))+two*((beta0-b(iact))*iact+a)
            Zln1_max=max(Zln1_max,Zln1)
            Aln1=Zln1+log(iact*one)
            A2ln1=Zln1+two*log(iact*one)
            Zln=addln(Zln,Zln1)
            Aln=addln(Aln,Aln1)
            A2ln=addln(A2ln,A2ln1)
          end if
          iact_old=iact
        end if
      end do
C
      if(iopt.ne.1) return
C
C iact probability density at beta0: (Zln1_max used only here.)
      iloop_ha=0
      hsum=zero
      do iact=0,nlink
        if(ha(iact).gt.half) then
          iloop_ha=iloop_ha+1
          if(iloop_ha.eq.1) then
            a=-(beta0-b(iact))*namin
          else
            a=a+(b(iact)-b(iact_old))*iact_old
          end if
          Zln1=log(ha(iact))+two*((beta0-b(iact))*iact+a)-Zln1_max
          if(Zln1.gt.0.01d00) stop "Zln1." 
          ha(iact)=exp(Zln1)
          hsum=hsum+ha(iact)
          iact_old=iact
        else
          ha(iact)=zero
        end if
      end do
      do iact=0,nlink
        ha(iact)=ha(iact)/hsum
      end do
C
      return
      end
