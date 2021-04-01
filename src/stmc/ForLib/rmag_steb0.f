      SUBROUTINE 
     & RMAG_STEB0(nrpt,iud,n0,ms,nqm1,mlink,ns,hm,hmm,hme,qm,ha)
C Copyright, Bernd Berg, Oct 15 2001.
C Reads nrpt times the the magnetization arrays hm() from unit iud and 
C calculates the histogram mean values hmm() and their error bars hme().
C Further, each of the nq magnetizations is calculated nrpt times in qm().
      include 'implicit.sta'
      include 'constants.par'
      dimension hm(n0:ms,n0:nqm1),hmm(n0:ms,n0:nqm1),hme(n0:ms,0:nqm1)
      dimension qm(nrpt,0:nqm1),ha(n0:mlink)
C
      do is=n0,ns
      do iq=n0,nqm1
        hmm(is,iq)=zero
      end do
      end do
      do irpt=1,nrpt
        read(iud) ha,acpt,irpt_in,nstate0,hm
        do iq=0,nqm1 ! potts_actm works also for the magnetization!
          qm(irpt,iq)=potts_actm(ns,hm(n0,iq))
        end do
        do iq=n0,nqm1
        do is=n0,ns
          hmm(is,iq)=hmm(is,iq)+hm(is,iq)
        end do
        end do
      end do
      do is=n0,ns
      do iq=n0,nqm1
        hmm(is,iq)=hmm(is,iq)/nrpt
      end do
      end do
C
      do irpt=1,nrpt
        backspace iud
      end do
C
      cut=half/nrpt
      do iq=n0,nqm1
      do is=n0,ns
        hme(is,iq)=zero
      end do
      end do
      do irpt=1,nrpt
        read(iud) ha,irpt_in,nstate0,hm
        do iq=n0,nqm1
        do is=n0,ns
        if(hmm(is,iq).gt.cut) then ! Prevents underflows.
          hme(is,iq)=hme(is,iq)+(hm(is,iq)-hmm(is,iq))**2
        end if
        end do
        end do
      end do
      do iq=n0,nqm1
      do is=n0,ns
        hme(is,iq)=sqrt(hme(is,iq)/(nrpt*(nrpt-1)))
      end do
      end do
C
      return
      end
