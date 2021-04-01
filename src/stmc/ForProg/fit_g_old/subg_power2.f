      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C Copyright, Bernd Berg, Apr 21 2002.
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION Y=A1*X**A2+A3*X**A4.
      include '../../ForLib/implicit.sta'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=A(1)*X**A(2)+A(3)*X**A(4)
      DYDA(1)=X**A(2)
      DYDA(2)=A(1)*LOG(X)*X**A(2)      
      DYDA(3)=X**A(4)
      DYDA(4)=A(3)*LOG(X)*X**A(4)      
      RETURN
      END
