      SUBROUTINE POTTS_Z0LN(nq,ns,nlink,namin,ndel_muca,b,ha,hasum,
     &                      Zln_dif)
C Copyright Bernd Berg, May 10 2002.
C Potts model, normalization of the partition function Z at beta0=0.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension b(0:nlink),ha(0:nlink),hasum(0:nlink)
      dimension ndel_muca(0:nlink) ! Eliminate in the final version!
C
      iloop_ha=0 ! Counts runs through the ha_iact.gt.half loop.
      beta0=zero
      Z0ln=ns*LOG(ONE*nq)
      if(namin<=0) stop "POTTS_ZLN: namin false."
      iact_next=namin+ndel_muca(namin) ! For test purposes only.
      do iact=0,nlink
        ha_iact=hasum(iact)-ha(iact) ! Jackknife histogram.
        if(ha_iact>half) then
          iloop_ha=iloop_ha+1
          if(iloop_ha==1) then
            a=-(beta0-b(iact))*namin ! Ferdinand-Fisher normalization.
            Zln=log(ha_iact)+two*((beta0-b(iact))*iact+a)
          else
            a=a+(b(iact)-b(iact_old))*iact_old
            if(iact>namin) then
              if(iact/=iact_next) then
                print'(" iact,_next,has:",2I10,2G15.6)',
     &                   iact,iact_next,hasum(iact),ha(iact)
                if(iact==nlink) stop "POTTS_Z0LN."
              end if
              iact_next=iact+ndel_muca(iact)
            end if
            Zln1=log(ha_iact)+two*((beta0-b(iact))*iact+a)
            Zln=addln(Zln,Zln1)
          end if
          iact_old=iact
        end if
      end do
      Zln_dif=Z0ln-Zln
C
      return
      end
