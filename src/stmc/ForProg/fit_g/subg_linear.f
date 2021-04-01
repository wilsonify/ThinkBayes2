 
      SUBROUTINE SUBG(X,A,YFIT,DYDA,NFIT)
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION Y=A1+A2*X (linear).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION A(NFIT),DYDA(NFIT)
      YFIT=A(1)+A(2)*X
      DYDA(1)=ONE
      DYDA(2)=X
      RETURN
      END

