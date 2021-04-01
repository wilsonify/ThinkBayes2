      subroutine o3_act(sta,ipf,ns,nd,act)
C Copyright, Bernd Berg, Feb 20 2002.
C Calculates action value iact of potts.com.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension sta(3,ns),ipf(nd,ns)
      act=zero
      do is=1,ns        
        do id=1,nd
          act=act+sta(1,is)*sta(1,ipf(id,is))+
     &            sta(2,is)*sta(2,ipf(id,is))+
     &            sta(3,is)*sta(3,ipf(id,is))
        end do
      end do
      return
      end
