      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C Bernd Berg, Apr 30 2004.
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION Y=A1+A2*X**A3.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=A(1)+A(2)*X**A(3)
      DYDA(1)=ONE
      DYDA(2)=X**A(3)
      DYDA(3)=A(2)*LOG(X)*X**A(3)      
      RETURN
      END
