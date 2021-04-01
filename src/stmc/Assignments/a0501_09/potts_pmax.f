      subroutine potts_pmax(nqmax)
      include 'implicit.sta'
C Metropolis updating with sequential spin choice.
      include 'lat.par'
      include 'potts.par'
      include 'potts1.com'
      dimension nstat(0:nqm1)
c
      do jp=0,nqm1
      nstat(jp)=0
      end do
c
      do is=1,ns
      nstat(ista(is))=nstat(ista(is))+1
      end do
c
      nqmax=0
      do jp=0,nqm1
      if(nstat(jp).gt.nqmax) nqmax=nstat(jp)
      end do
c
      return
      end
