      subroutine p_mu_rec
C Copyright: Bernd Berg, Nov 25, 2000.
C Recursion of multicanonical weights.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
C
C Weight changes outside the [namin,namax] range forbidden:
      if(ia_min.lt.namin) then
        do ia=ia_min,(namin-iastep),iastep
          hmuca(ia)=hmuca(ia)+ha(ia)
          ha(ia)=zero
        end do
        ia_min=namin ! Re-definition of ia_min for this case.
      end if
C
      if(ia_max.gt.namax) then
        do ia=(namax+iastep),ia_max,iastep
          hmuca(ia)=hmuca(ia)+ha(ia)
          ha(ia)=zero
        end do
        ia_max=namax ! Re-definition of ia_max for this case.
      end if
C
      hmuca(ia_min)=hmuca(ia_min)+ha(ia_min)
      hup(ia_min)=hup(ia_min)+ha(ia_min)
      hdn(ia_min)=hdn(ia_min)+ha(ia_min)
      ha(ia_min)=zero
      if(ia_max.eq.ia_min) stop "p_mu_rec: ia_max.eq.ia_min."
C
      ia1=ia_min
      ia=ia_min
1     ia=ia+iastep ! tame label 1.
        hmuca(ia)=hmuca(ia)+ha(ia)
        hup(ia)=hup(ia)+ha(ia)
        hdn(ia)=hdn(ia)+ha(ia)
        ha(ia)=zero
        if(hmuca(ia).gt.half) then
          ndel_new=ia-ia1
          ndel_old=ndel_muca(ia1)
          if(ndel_old.gt.ndel_new) then
            gw(ia1)=zero
            ndel_muca(ia1)=ndel_new
            ndel_muca(ia)=ndel_old-ndel_new
          end if
          ia1=ia
        end if
      if(ia.lt.ia_max) goto 1 ! Only goto to label 1.
C
C     Calculation of the smallest action considered, ia1:
      ia1=ia_min
      iatest=ia_min
2     iatest=iatest-iastep ! Tame label 2.
      if(iatest.ge.namin) then
        if(iatest+ndel_muca(iatest).eq.ia_min) ia1=iatest
      else
        if(iatest.gt.ia_min-n2d) goto 2 ! Only goto to label 2.
      end if
C
C Weight factor recursion:
C
      ia2step=2*iastep
      do ia=ia1,ia_max,iastep
        idel_muca=ndel_muca(ia)
        ia_up=ia+idel_muca
        hdn_a=hdn(ia)
        hup_a=hup(ia_up)
        if(hup_a.gt.half .and. hdn_a.gt.half) then ! if 1
C         The new weight factor:
C         g-weights:
          gw0=hdn_a*hup_a/(hdn_a+hup_a)
          gw00=gw0/(gw(ia)+gw0) ! normalized.
          gw(ia)=gw(ia)+gw0     ! g-weight iteration.
          wfactor=wrat(idel_muca,ia)*(hdn_a/hup_a)**gw00
          hdn(ia)=zero
          hup(ia_up)=zero
          if(idel_muca.eq.iastep) then ! if 2
            wrat(iastep,ia)=wfactor
            wrat(-iastep,ia+iastep)=one/wfactor
            iab=ia
            do id=ia2step,n2d,iastep
              iab=iab-iastep
              wrat(id,iab)=wrat(id-iastep,iab)*wfactor
              wrat(-id,iab+id)=one/wrat(id,iab)
            end do
          else ! if 2
            if(idel_muca.lt.iastep) stop "p_mu_rec: idel_muca error."
            wfactor=wfactor**((iastep*one)/(idel_muca*one))
            nstep=(idel_muca/iastep)
            iaa=ia-iastep
            do istep=1,nstep
              iaa=iaa+iastep
              wrat(iastep,iaa)=wfactor
              wrat(-iastep,iaa+iastep)=one/wfactor
              iab=iaa
              do id=ia2step,n2d,iastep
                iab=iab-iastep
                wrat(id,iab)=wrat(id-iastep,iab)*wfactor
                wrat(-id,iab+id)=one/wrat(id,iab)
              end do
            end do
          end if ! if 2
        end if ! if 1
      end do
C
C Weight factors for the largest action values encountered:
      ia2=ia_max-iastep
      ia1=ia_max-n2d+iastep
      iastep1=iastep
      do ia=ia2,ia1,-iastep
        iastep1=iastep1+iastep
        do id=iastep1,n2d,iastep
          wrat(id,ia)=wrat(id-iastep,ia)*wrat(iastep,ia)
          wrat(-id,ia+id)=one/wrat(id,ia)
        end do
      end do
C
      ia_max=0
      ia_min=mlink
C
      return
      end

