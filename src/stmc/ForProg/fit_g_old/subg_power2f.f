      SUBROUTINE SUBG(X,A,YFIT,DYDA,NFIT)
C Copyright, Bernd Berg, Feb 8 2002.
C USER PROVIDED SUBROUTINE THE FOR THE FIT FUNCTION
C                           Y=A1*X**A2*(1+A3*X**A4).
C Useful as 4 Paramter fit for critical exponents.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION A(NFIT),DYDA(NFIT)
      IF(NFIT/=4) STOP "subg_exp4.f: NFIT false."
      YFIT=   A(1)*X**A(2)*(ONE+A(3)*X**A(4))
      DYDA(1)=     X**A(2)*(ONE+A(3)*X**A(4))
      DYDA(2)=A(1)*LOG(X)*X**A(2)*(ONE+A(3)*X**A(4))
      DYDA(3)=A(1)*X**A(2)*X**A(4)
      DYDA(4)=A(1)*X**A(2)*A(3)*LOG(X)*X**A(4)
      RETURN
      END
