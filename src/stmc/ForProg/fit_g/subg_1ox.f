      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION Y=A1+A2/X.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=A(1)+A(2)/X
      DYDA(1)=ONE
      DYDA(2)=ONE/X
      RETURN
      END
