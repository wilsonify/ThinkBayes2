      SUBROUTINE BIN_INTA(K,KBMAX,DATA,DATB,DBE,RUP,RDO,RAT)
C Copyright: Bernd Berg, Feb 18 2000.
C Caculation of the integrated autocorrelation time tau_int via the 
C bining procedure: The variance ratio RAT converges towards tau_int.  
C RUP and RDO are analytic upper and lower confidence limits which
C converge toward the one error bar range.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION DATA(2**K),DATB(2**K)
      DIMENSION DBE(0:KBMAX),RUP(0:KBMAX),RDO(0:KBMAX),RAT(0:KBMAX)
      NDAT=2**K
      DO KB=0,KBMAX
        NBIN=2**KB
        NBINS=NDAT/NBIN
        CALL BINING(NDAT,DATA,NBINS,DATB)
        CALL STEB0(NBINS,DATB, DBMEAN,DBV,DBE(KB))
        RAT(KB)=(DBE(KB)/DBE(0))**2
C Confidence limits as substitute for the two e-bar range. 
        PC=-ONE
        CALL SEBAR_E(NBINS,PC,EBUP,EBDO)
        SRT=SQRT(RAT(KB))
        SRTUP=SRT*EBUP
        SRTDO=SRT*EBDO
        SRTUP=SRT+HALF*(SRTUP-SRT)
        SRTDO=SRT-HALF*(SRT-SRTDO)
        RUP(KB)=SRTUP**2
        RDO(KB)=SRTDO**2
      END DO
      RETURN
      END
