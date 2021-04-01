      SUBROUTINE RWGHT_TS_ZLNJ(NRPT,ZlnA,AlnA,A2lnA,ZlnJ,AlnJ,A2lnJ) 
C BB, Nov 29 2003. Temperature re-weighting of an action time series:
C                  Jackknife bins.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION ZlnA(nrpt),AlnA(nrpt),A2lnA(nrpt)
      DIMENSION ZlnJ(0:nrpt),AlnJ(0:nrpt),A2lnJ(0:nrpt)

      ZlnJ(0)=ZlnA(1)
      AlnJ(0)=AlnA(1)
      A2lnJ(0)=A2lnA(1)
      DO IRPT=2,NRPT
        Zlnj(0)=ADDLN(ZlnJ(0),ZlnA(irpt))
        Alnj(0)=ADDLN(AlnJ(0),AlnA(irpt))
        A2lnj(0)=ADDLN(A2lnJ(0),A2lnA(irpt))
      END DO

      DO IRPT=1,NRPT
        ISGN=-1
        CALL ADDLN2(ZlnJ(0),ZlnA(IRPT),ZlnJ(IRPT),ISGN)
        if(isgn.ne.1) stop "RWG_TS_ZLNJ: Zlnj negativ."
        isgn=-1
        call addln2(Alnj(0),Alna(irpt),Alnj(irpt),isgn)
        if(isgn.ne.1) stop "RWG_TS_ZLNJ: Alnj negativ."
        isgn=-1
        call addln2(A2lnj(0),A2lna(irpt),A2lnj(irpt),isgn)
        if(isgn.ne.1) stop "RWG_TS_ZLNJ: A2lnj negativ."
      END DO

      RETURN
      END
