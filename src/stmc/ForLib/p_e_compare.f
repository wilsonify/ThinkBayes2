      subroutine p_e_compare(nact_old,nact_new,n2d,mcase,ncase,icase)
      logical lold
C Copyright Bernd Berg, Jan 02 2002. 
C Purpose: Count distinct local energy distributions as needed for EDS.
      dimension nact_old(0:n2d+1,mcase),nact_new(0:n2d+1)
c
      n2dp1=n2d+1
      if(ncase.ge.1) then
        do icase=1,ncase
          lold=.true.
          do id=0,n2dp1
            if(nact_old(id,icase).ne.nact_new(id)) lold=.false.
          end do
          if(lold) return
        end do
      end if
c
      ncase=ncase+1
c     print'(" p_e_compare: ncase,nact=",I4,4X,8I4)',ncase,nact_new
      do id=0,n2dp1
        nact_old(id,ncase)=nact_new(id)
      end do
      icase=ncase+1
      return
      end
