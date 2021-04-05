      FUNCTION LPT_EX_IA(BETA1,BETA2,IACT1,IACT2)
C Copyright, Bernd Berg, Jan 8 2002.
C Parallel tempering: Acceptance or Rejection of beta exchange (IACT version).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      LPT_EX_IA=.FALSE.
      DELH=(BETA2-BETA1)*(IACT2-IACT1)
      IF(RMAFUN()<EXP(-DELH)) LPT_EX_IA=.TRUE.
      RETURN
      END
