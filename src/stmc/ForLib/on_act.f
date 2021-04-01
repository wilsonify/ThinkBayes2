      subroutine On_act(sta,ipf,n,ns,nd,act)
C Copyright, Bernd Berg, Feb 12 2002.
C Calculates action value iact of potts.com.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension sta(n,ns),ipf(nd,ns)
      act=zero
      do is=1,ns        
        do id=1,nd
          do i=1,n
            act=act+sta(i,is)*sta(i,ipf(id,is))
          end do
        end do
      end do
      return
      end
