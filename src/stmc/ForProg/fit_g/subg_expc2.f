      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C Bernd Berg, Apr 30 2004.
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION 
C Y=A1+A2*EXP(A3*X)+A4*EXP(A5*X).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=A(1)+A(2)*EXP(A(3)*X)+A(4)*EXP(A(5)*X)
      DYDA(1)=ONE
      DYDA(2)=EXP(A(3)*X)
      DYDA(3)=A(2)*X*EXP(A(3)*X)
      DYDA(4)=EXP(A(5)*X)
      DYDA(5)=A(4)*X*EXP(A(5)*X)
      RETURN
      END
