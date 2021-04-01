      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C Copyright, Bernd Berg, Feb 8 2002.
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION Y=A1*X**A2.
      include '../../ForLib/implicit.sta'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=A(1)*X**A(2)
      DYDA(1)=X**A(2)
      DYDA(2)=A(1)*LOG(X)*X**A(2)      
      RETURN
      END
