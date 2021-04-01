      FUNCTION F_QDF(F)
C VARIANCE RATIO DISTRIBUTION FUNCTION.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /PARFT/ NF1,NF2
      XF1H=HALF*NF1
      XF2H=HALF*NF2
      X=(NF2*ONE)/(NF1*F+NF2)
      F_QDF=ONE-BETA_I(X,XF2H,XF1H)
      IF(F_QDF.GT.HALF) F_QDF=ONE-F_QDF
      RETURN
      END
