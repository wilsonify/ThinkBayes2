      SUBROUTINE DATJACK2(N,X,XJJ)
C Copyright Bernd Berg, Dec 13 2000.
C CALCULATION OF  SECOND LEVEL JACKKNIFE BINS  XJJ(N-1,N)
C FROM  N  DATA  X(N).                          
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION X(N),XJJ(N-1,N)
C
      XSUM=ZERO
      DO I=1,N
        XSUM=XSUM+X(I)
      END DO
C
      NM1=N-1
      FACTOR=ONE/(N-2)
      DO I=1,N
      DO J=1,NM1
        JJ=J
        IF(J.GE.I) JJ=J+1
        XJJ(J,I)=FACTOR*(XSUM-X(I)-X(JJ))
      END DO
      END DO
      RETURN
      END
