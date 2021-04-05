      FUNCTION CHI2_XQ(Q)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      EXTERNAL CHI2_DF
      COMMON /CHI2PAR/ NF
      X1=ZERO
      X2=ZERO
      Q1=chi2_df(x1)
1     X2=X2+ONE*NF
      QT=CHI2_DF(X2)
      IF(QT<=Q) GO TO 1
      Q2=chi2_df(x2)
      CHI2_XQ=FI1(CHI2_DF,Q,X1,X2)
      RETURN
      END
