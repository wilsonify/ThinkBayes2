      SUBROUTINE AUTCORJ(IT,NDAT,NBINS,DATA,WORK,ACORJ,LMEAN)
C Copyright, Bernd Berg, Feb 11, 2001.
C Calculates the jackknife array of autocorrelation at IT (0\le IT).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION DATA(NDAT),WORK(NBINS),ACORJ(NBINS)
c
      DMEAN=ZERO
      IF(LMEAN) DMEAN=STMEAN(NDAT,DATA)
      NN=NDAT-IT
c
      NBIN=NN/NBINS
      NNJ=NBINS*NBIN-NBIN
      IF(NBIN<=1) STOP "AUTCORJ: NBIN.LE.1!"
      IF(NNJ<=IT) STOP "AUTCORJ: NNJ.LE.IT!"
      DO IBINS=1,NBINS
        WORK(IBINS)=ZERO
        I1=1+(IBINS-1)*NBIN
        I2=IBINS*NBIN
        DO I=I1,I2
          WORK(IBINS)=WORK(IBINS)+(DATA(I)-DMEAN)*(DATA(I+IT)-DMEAN)
        END DO    
      WORK(IBINS)=WORK(IBINS)/NBIN
      END DO    
      CALL DATJACK(NBINS,WORK,ACORJ)
C Correction of bias from DMEAN:
      IF(LMEAN) THEN
      DO IBINS=1,NBINS
        ACORJ(IBINS)=(ACORJ(IBINS)*NDAT)/(NDAT-1)
      END DO    
      END IF
c
      RETURN
      END
