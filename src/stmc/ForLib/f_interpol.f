      FUNCTION F_INTERPOL(N,F,X,XX)
C Copyright, Bernd Berg, July 9 2001.
C Interpolation of a function given by the function array F() and
C the strictly monoton and ardered argument array X(), X(1)<...<X(N).
      include '../../ForLib/implicit.sta'
      DIMENSION F(N),X(N)
      IF(XX<X(1).OR.XX>X(N)) THEN
	PRINT'(" F_INTERPOL, X(1),XX,X(N):",3G16.7)',X(1),XX,X(N)
	STOP "F_INTERPOL: XX out of range."
      END IF
      I=0
1     I=I+1
      IF(XX>X(I)) GO TO 1
      F_INTERPOL=((X(I)-XX)*F(I-1)+(XX-X(I-1))*F(I))/(X(I)-X(I-1))
      RETURN
      END
