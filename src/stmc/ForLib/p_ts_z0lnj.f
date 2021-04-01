      SUBROUTINE P_TS_Z0LNJ(ns,nq,nrpt,Zlna,Zlnj,Zlnj_dif)
C Copyright Bernd Berg, Jul 9 2000.  Potts model, jackknife 
C       time series partition function fractions at beta0=0.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension Zlna(nrpt),Zlnj(0:nrpt),Zlnj_dif(0:nrpt)
C
      Zlnj(0)=Zlna(1)
      do irpt=2,nrpt
        Zlnj(0)=addln(Zlnj(0),Zlna(irpt))
      end do
C
      do irpt=1,nrpt
        isgn=-1
        call addln2(Zlnj(0),Zlna(irpt),Zlnj(irpt),isgn)
        if(isgn.ne.1) stop "P_TS_Z0LNJ: Zlnj negativ."
      end do
C
      Zln0=ns*LOG(ONE*nq)
      do irpt=0,nrpt
        Zlnj_dif(irpt)=Zln0-Zlnj(irpt) ! =Zln(beta=0)-Zln.
      end do
C
      return
      end
