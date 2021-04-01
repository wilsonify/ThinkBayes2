      subroutine potts_clw(nclsize)
C Copyright, Bernd Berg, Oct 17 2001. 
C Wolff cluster updating for q-state Potts models in d dimensions.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/potts1.com'
      include '../../ForLib/lat.com'
      dimension iacl(ms)
C
      qm1=one*nqm1
      boltz1=one-wrat(-1)
      nclsize=1 ! Cluster size.
      icl=1     ! Presently considered cluster variable.
      call ranmar(xr)
      is=1+int(ns*xr)               ! Random selection of a site.
      iqold=ista(is)                ! Present state.
      call ranmar(xr)
      iqnew=int(qm1*xr)
      if(iqnew.ge.iqold) iqnew=iqnew+1 ! New state.
1     continue
        ista(is)=iqnew
        do id=1,nd
          isf=ipf(id,is)            ! Forward direction.
          iqtest=ista(isf)
          if(iqold.eq.iqtest) then
            iact=iact-1
            call ranmar(xr)
            if(xr.lt.boltz1) then
              nclsize=nclsize+1
              iacl(nclsize)=isf
              ista(isf)=-1
            end if
          else
            if(iqnew.eq.iqtest) iact=iact+1
            if(-1.eq.iqtest) iact=iact-1
          end if
          isb=ipb(id,is)            ! Backward direction.
          iqtest=ista(isb)
          if(iqold.eq.iqtest) then
            iact=iact-1
            call ranmar(xr)
            if(xr.lt.boltz1) then
              nclsize=nclsize+1
              iacl(nclsize)=isb
              ista(isb)=-1
            end if
          else
            if(iqnew.eq.iqtest) iact=iact+1
            if(-1.eq.iqtest) iact=iact-1
          end if
        end do
        icl=icl+1
        if(icl.le.nclsize) then
          is=iacl(icl)
      go to 1
        end if
      ha(iact)=ha(iact)+one ! Action histogram.
      return
      end
