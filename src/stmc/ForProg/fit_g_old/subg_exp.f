      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C Copyright, Bernd Berg, Sep 26 2002.
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION Y=A1*EXP(A2*X).
      include '../../ForLib/implicit.sta'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=A(1)*EXP(A(2)*X)
      DYDA(1)=EXP(A(2)*X)
      DYDA(2)=A(1)*X*EXP(A(2)*X)
      RETURN
      END
