      SUBROUTINE AUTOJ_TAU(IUO,NBINS,AJ)
C Jackknife analysis of the exponential and integrated 
C autocorrelation times. Input: coefficients from jackknifed fits.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(NB_MAX=256)
      DIMENSION TAUJ_EXP(0:NB_MAX),TAUJ_INT(0:NB_MAX),AJ(0:NBINS) 
      IF(NBINS>NB_MAX) STOP "AUTO_TAU: Enlarge NB_MAX!"
      DO IBIN=0,NBINS
        TAUJ_EXP(IBIN)=-ONE/AJ(IBIN) 
        TAUJ_INT(IBIN)=ONE+TWO*EXP(AJ(IBIN))/(ONE-EXP(AJ(IBIN)))
      END DO
      CALL STEBJ0(NBINS,TAUJ_EXP(1),TAUM,TAUV,TAUE)
      WRITE(IUO,'(/," TAUM_EXP =",F14.8," +/=",F14.8)') TAUM,TAUE
      BIAS=TAUJ_EXP(0)-TAUM
      WRITE(IUO,'(" Bias estimate =",F14.8)') BIAS
      CALL STEBJ0(NBINS,TAUJ_INT(1),TAUM,TAUV,TAUE)
      WRITE(IUO,'(/," TAUM_INT =",F14.8," +/=",F14.8)') TAUM,TAUE
      BIAS=TAUJ_INT(0)-TAUM
      WRITE(IUO,'(" Bias estimate =",F14.8)') BIAS
      RETURN
      END
