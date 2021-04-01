      subroutine potts_cl(ncl)
C Bernd Berg, August 26, 2001. Swendsen-Wang cluster updating.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'potts.par'
      include '../../ForLib/potts1.com'
      include '../../ForLib/lat.com'
      dimension iacl(ms),ianew(ms)
c
      q=nq*one
      boltz1=one-wrat(-1)
c
      do is=1,ns     ! Initialize arrays.
        iacl(is)=-1  ! Array of cluster numbers at the sites.
        ianew(is)=-1 ! Array for cluster re-labelling. Later new states.
      end do
c
      ncl=0 ! Number of clusters.
      icl=0 ! Temporary cluster label.
      do is=1,ns ! Loop over all sites.
        if(iacl(is).eq.-1) then
          ncl=ncl+1
          icl=icl+1
          inew=icl
          ianew(icl)=icl  ! Initial cluster label.
          iacl(is)=icl    ! Initial cluster label at is.
        else
          inew=iacl(is)                ! Cluster label found for is.
2         if(inew.ne.ianew(inew)) then ! Iterate down to
             inew=ianew(inew)          ! a smaller label.
             go to 2
          end if
        end if
        do id=1,nd       ! Loop over all directions.
          lbond=.false.  ! Initialize the bond variable.
          isf=ipf(id,is) ! Forward directions as we cover all sites.
          if(ista(is).eq.ista(isf)) then  ! The site may be joined.
            call ranmar(xr)
            if(xr.lt.boltz1) lbond=.true. ! If true, the site is joined.
          endif
          if(lbond) then ! If needed use a bond array.
            iclf=iacl(isf)
            if(iclf.eq.-1) then
              iacl(isf)=inew ! Intial cluster label at isf set.
            else
3             if(iclf.ne.ianew(iclf)) then ! Iterate down to
                 iclf=ianew(iclf)          ! a smaller label.
                 go to 3
              end if
              if(inew.ne.iclf) then ! Identify inew or iclf
                ncl=ncl-1           ! with a smaller cluster label.
                imin=min(inew,iclf)
                ianew(max(inew,iclf))=imin
                inew=imin
              end if
            end if
          end if
        end do
      end do
      icl1=icl ! Number of presenlty assigned cluster labels.
c
c Create final cluster labelling:
      do is=1,ns ! Loop over all sites again.
        icl=iacl(is)
1       inew=ianew(icl)
        if(inew.ne.icl) then ! Iterate down to the smallest label.
          icl=inew
          go to 1
        end if
        iacl(is)=inew ! Only cluster labels which fullfill the
      end do          ! equation icl=ianew(ilc) are left over.
c
      ndif=0
      do icl=1,icl1          ! Reduce the set of cluster labels to a
        inew=ianew(icl)      ! permutation of the numbers 1,...,ncl.
        if(inew.ne.icl) then
          ndif=ndif+1
        else
          ianew(icl)=inew-ndif
        end if
      end do
c
      do is=1,ns
        icl=iacl(is)
        iacl(is)=ianew(icl)  ! Final cluster label at the site is.
      end do
c
      do icl=1,ncl
        call ranmar(xr)
        ianew(icl)=int(q*xr) ! Proposals for the cluster states.
      end do
c
      do is=1,ns
        ista(is)=ianew(iacl(is)) ! Update clusters.
      end do
c
c action
      call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)
      ha(iact)=ha(iact)+one
      return
      end
