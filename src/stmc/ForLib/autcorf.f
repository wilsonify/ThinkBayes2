      FUNCTION AUTCORF(IT,NDAT,DATA,LMEAN)
C Copyright, Bernd Berg, Feb 11 2001.
C The function calculates the autocorrelation at IT from the input array 
C DATA of autocorrelations. Allowed values of IT are IT=0,1, ... < NDAT.
      include 'implicit.sta'
      include 'constants.par'
      DIMENSION DATA(NDAT)
      DMEAN=ZERO
      IF(LMEAN) DMEAN=STMEAN(NDAT,DATA)
      NN=NDAT-IT
      AUTCORF=ZERO
      DO I=1,NN
        AUTCORF=AUTCORF+(DATA(I)-DMEAN)*(DATA(I+IT)-DMEAN)
      END DO    
      IF(LMEAN) AUTCORF=AUTCORF/(NN*ONE)*((NDAT*ONE)/(NDAT*ONE-ONE))
      IF(.NOT.LMEAN) AUTCORF=AUTCORF/(NN*ONE)
      RETURN
      END
