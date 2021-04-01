      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C Copyright, Bernd Berg, Oct 4 2002.
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION 
C Y=A1*EXP(A2*X)+A3*EXP(A4*X).
      include '../../ForLib/implicit.sta'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=A(1)*EXP(A(2)*X)+A(3)*EXP(A(4)*X)
      DYDA(1)=EXP(A(2)*X)
      DYDA(2)=A(1)*X*EXP(A(2)*X)
      DYDA(3)=EXP(A(4)*X)
      DYDA(4)=A(3)*X*EXP(A(4)*X)
      RETURN
      END
