      SUBROUTINE P_TS_ZLNJ(nq,nrpt,Zlna,Alna,A2lna,XM2lna,
     &                             Zlnj,Alnj,A2lnj,XM2lnj)
C Copyright Bernd Berg, Apr 9 2004.  Potts model, jackknife 
C       time series partition function fractions.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension Zlna(nrpt),Alna(nrpt),A2lna(nrpt),XM2lna(nrpt),
     &  Zlnj(0:nrpt),Alnj(0:nrpt),A2lnj(0:nrpt),XM2lnj(0:nrpt)
C
      nqm1=nq-1
      Zlnj(0)=Zlna(1)
      Alnj(0)=Alna(1)
      A2lnj(0)=A2lna(1)
      XM2lnj(0)=XM2lna(1)
      do irpt=2,nrpt
        Zlnj(0)=addln(Zlnj(0),Zlna(irpt))
        Alnj(0)=addln(Alnj(0),Alna(irpt))
        A2lnj(0)=addln(A2lnj(0),A2lna(irpt))
        XM2lnj(0)=addln(XM2lnj(0),XM2lna(irpt))
      end do
C
      do irpt=1,nrpt
        isgn=-1
        call addln2(Zlnj(0),Zlna(irpt),Zlnj(irpt),isgn)
        if(isgn.ne.1) stop "P_TS_ZLNJ: Zlnj negativ."
        isgn=-1
        call addln2(Alnj(0),Alna(irpt),Alnj(irpt),isgn)
        if(isgn.ne.1) stop "P_TS_ZLNJ: Alnj negativ."
        isgn=-1
        call addln2(A2lnj(0),A2lna(irpt),A2lnj(irpt),isgn)
        if(isgn.ne.1) stop "P_TS_ZLNJ: A2lnj negativ."
        do iq=0,nqm1
          isgn=-1
          call addln2(XM2lnj(0),XM2lna(irpt),XM2lnj(irpt),isgn)
          if(isgn.ne.1) stop "P_TS_ZLNJ: XM2lnj negativ."
        end do
      end do
C
      return
      end
