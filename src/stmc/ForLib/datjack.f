      SUBROUTINE DATJACK(N,X,XJ)
C Copyright, Bernd Berg, Dec 13 2000.
C CALCULATION OF  N  JACKKNIFE BINS  XJ(N)  FOR  N  DATA IN  X(N).        
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION X(N),XJ(N)
      XSUM=ZERO
      DO I=1,N
        XSUM=XSUM+X(I)
      END DO
      FACTOR=ONE/(N-1)
      DO I=1,N
        XJ(I)=FACTOR*(XSUM-X(I))
      END DO
      RETURN
      END
