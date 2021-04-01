      subroutine potts_mag(ns,nqm1,ista,nstate)
C Copyright Bernd Berg, Jul 9 2002.
C Magnetization measurement for the Potts model.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension ista(ns),nstate(0:nqm1)
      ltest=.true.
      do iq=0,nqm1
        nstate(iq)=0
      end do
      do is=1,ns
        nstate(ista(is))=nstate(ista(is))+1
      end do
      return
      end
