      program p_e_cases
C Copyright, Bernd Berg, Jan 19 2002.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter (iuo=6,nd_max=3,nq_max=8,mcase=80,n2d_max=3*nd_max)
      dimension iqarray(n2d_max),nqa(n2d_max)
      dimension nact_old(0:n2d_max+1,mcase),nact_new(0:n2d_max+1)
C
      do nd=2,nd_max
        n2d=2*nd
        n2dp1=n2d+1
	write(iuo,*) " "
C
        do nq=2,nq_max
          nqm1=nq-1
          nconf0=nq**n2d
C
          do id=1,n2d
            nqa(id)=nq ! Sets the nq-box dimensions for the ixcor call.
          end do
C
          ncase=0
          do iconf0=1,nconf0 ! Loop over the neighbor configurations.
            call ixcor(iqarray,nqa,iconf0,n2d)
            do iq_in=0,nqm1   ! Loop over the central state.
              do id=0,n2d
                nact_new(id)=0
              end do
              iconf=iconf0+iq_in*nconf0
              do iq=0,nqm1
                ia=0
                do id=1,n2d
                  if(iq.eq.iqarray(id)) ia=ia+1
                end do
                if(iq.ne.iq_in) nact_new(ia)=nact_new(ia)+1
                if(iq.eq.iq_in) nact_new(n2dp1)=ia
              end do
              call p_e_compare(nact_old,nact_new,n2d,mcase,ncase,icase)
              if(ncase.gt.mcase) stop "p_e_cases: enlarge mcase!"
            end do
          end do
          write(iuo,'(" nd,nq,ncase:",2I4,I10)') nd,nq,ncase
        end do
      end do
C
      stop
      end


      subroutine p_e_compare(nact_old,nact_new,n2d,mcase,ncase,icase)
C Copyright Bernd Berg, Jan 02 2002.
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
c
      icase=ncase+1
      return
      end

      include '../../ForLib/ixcor.f'
