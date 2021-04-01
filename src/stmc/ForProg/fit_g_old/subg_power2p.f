      SUBROUTINE SUBG(X,A,YFIT,DYDA,MFTO)
C Copyright, Bernd Berg, Apr 21 2002.
C USER PROVIDED SUBROUTINE FOR THE FIT FUNCTION
C Y=C1*X**A2+C3*X**A4 with positive coefficients C1=EXP(A1) and C3=EXP(A3).
      include '../../ForLib/implicit.sta'
      DIMENSION A(MFTO),DYDA(MFTO)
      YFIT=EXP(A(1))*X**A(2)+EXP(A(3))*X**A(4)
      DYDA(1)=EXP(A(1))*X**A(2)
      DYDA(2)=EXP(A(1))*LOG(X)*X**A(2)      
      DYDA(3)=EXP(A(3))*X**A(4)
      DYDA(4)=EXP(A(3))*LOG(X)*X**A(4)      
      RETURN
      END
