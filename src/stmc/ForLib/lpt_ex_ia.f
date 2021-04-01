      FUNCTION LPT_EX_IA(BETA1,BETA2,IACT1,IACT2)
      include 'implicit.sta'
      include 'constants.par'
      LPT_EX_IA=.FALSE.
      DELH=(BETA2-BETA1)*(IACT2-IACT1)
      IF(RMAFUN().LT.EXP(-DELH)) LPT_EX_IA=.TRUE.
      RETURN
      END
