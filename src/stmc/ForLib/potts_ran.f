      subroutine potts_ran(ista,ns,nq)
C Copyright, Bernd Berg, Nov 10 2000.
C Assigns random (i.e. beta=0) values 0,..,nq-1 to the states ista(is).
      include 'implicit.sta'
      include 'constants.par'
      dimension ista(ns)
      q=one*nq
      do is=1,ns
        call ranmar(xr)
        ista(is)=int(q*xr)
      end do
      return
      end
