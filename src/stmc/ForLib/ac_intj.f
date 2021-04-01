      SUBROUTINE AC_INTJ(NT,NBINS,ACORJ,ACINTJ)
C Copyright, Bernd Berg, Feb 11 2001.
C Input:  Jackknife array of autocorrelations ACORJ.
C Output: Jackknife array of integrated autocorrelation times ACINTJ.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION ACORJ(NBINS,0:NT),ACINTJ(NBINS,0:NT)
      DO IBINS=1,NBINS
      ACINTJ(IBINS,0)=ONE
        DO IT=1,NT
        ACINTJ(IBINS,IT)=ACINTJ(IBINS,IT-1)+TWO*ACORJ(IBINS,IT)
     &                                         /ACORJ(IBINS,0)
        END DO
      END DO
      RETURN
      END
