      subroutine XY_act1(sta,ipf,ns,nd,act)
C Calculates action value iact of potts.com.
      include 'implicit.sta'
      include 'constants.par'
      dimension sta(ns),ipf(nd,ns)
      act=zero
      do is=1,ns
        ph1=sta(is)
        do id=1,nd
          act=act+cos(abs(sta(ipf(id,is))-ph1))
        end do
      end do
      return
      end
